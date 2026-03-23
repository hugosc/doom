;;; org-annotation-hover.el --- Hover previews for org-roam id: links -*- lexical-binding: t -*-

;; Lightweight hover previews for org id: links (org-roam annotations)
;; Place this file in ~/.config/doom/lisp/ and (add-to-list 'load-path "~/.config/doom/lisp/")
;; Then (require 'org-annotation-hover) from your config.org or config.el

;;; Commentary:
;; - Adds a capture template for annotations
;; - Provides a node-find wrapper excluding annotation-tagged nodes
;; - org-annotation-hover-mode: overlays with help-echo previews (cached)
;; - Optional posframe support when `org-annotation-hover-use-posframe' is non-nil

;;; Code:

(require 'org)
(require 'subr-x)
(eval-when-compile (require 'cl-lib))

(defgroup org-annotation-hover nil
  "Hover previews for org-roam id: links."
  :group 'org)

(defcustom org-annotation-hover-annotations-dir "annotations/"
  "Directory (relative to `org-directory' / `org-roam-directory') used for annotations.
Used mainly for helpers if you want directory-based exclusion."
  :type 'string
  :group 'org-annotation-hover)

(defcustom org-annotation-hover-annotation-tag "annotation"
  "Tag used to mark annotation nodes."
  :type 'string
  :group 'org-annotation-hover)

(defcustom org-annotation-hover-use-posframe nil
  "When non-nil, attempt to render hover previews in a posframe (if posframe is installed).
If nil, fall back to `help-echo' tooltips only."
  :type 'boolean
  :group 'org-annotation-hover)

(defcustom org-annotation-hover-max-preview-lines 15
  "Maximum number of lines to show in a hover preview."
  :type 'integer
  :group 'org-annotation-hover)

(defcustom org-annotation-hover-enable-filewatch nil
  "If non-nil, set up a file-notify watcher on `org-roam-directory' to clear cache on external changes.
Requires `file-notify-add-watch' support."
  :type 'boolean
  :group 'org-annotation-hover)

(defvar org-annotation-hover--preview-cache (make-hash-table :test 'equal)
  "Cache mapping ID -> plist (:text STRING :file FILE).")

(defvar-local org-annotation-hover--overlays nil
  "List of overlays installed by org-annotation-hover-mode in the current buffer.")

(defvar org-annotation-hover--file-watch-descriptor nil
  "File-notify descriptor if the watcher is active.")

;;; Helpers: cache management

(defun org-annotation-hover--clear-cache (&optional file)
  "Clear preview cache.
If FILE is non-nil, only remove entries associated with that FILE (absolute path)."
  (if file
      (let ((keys (hash-table-keys org-annotation-hover--preview-cache)))
        (dolist (k keys)
          (let ((val (gethash k org-annotation-hover--preview-cache)))
            (when (and (plist-get val :file)
                       (string= (expand-file-name (plist-get val :file))
                                (expand-file-name file)))
              (remhash k org-annotation-hover--preview-cache)))))
    (clrhash org-annotation-hover--preview-cache)))

(defun org-annotation-hover--cache-put (id text file)
  "Store TEXT for ID with FILE in the preview cache."
  (puthash id (list :text text :file (and file (expand-file-name file)))
           org-annotation-hover--preview-cache))

(defun org-annotation-hover--cache-get (id)
  "Return plist stored for ID or nil."
  (gethash id org-annotation-hover--preview-cache))

;;; Preview generation

(defun org-annotation-hover--extract-subtree-as-plain (file pos &optional max-lines)
  "Open FILE and extract the subtree at POS, returning plain text.
If extraction fails, return a short placeholder. MAX-LINES limits lines."
  (let ((max-lines (or max-lines org-annotation-hover-max-preview-lines)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; If pos is a cons (file . position) then accept its second as position
        (when (number-or-marker-p pos)
          (goto-char pos))
        (ignore-errors (org-back-to-heading t))
        (let ((beg (point))
              (end (ignore-errors (org-end-of-subtree t t))))
          (unless end (setq end (min (point-max) (+ beg 2000)))) ; fallback
          (let* ((raw (buffer-substring-no-properties beg (min end (+ beg 2000))))
                 (exported
                  (condition-case _
                      ;; Prefer org's ascii exporter if available
                      (when (fboundp 'org-export-string-as)
                        (org-export-string-as raw 'ascii t '(:body-only t)))
                    (error nil))))
            (let ((s (string-trim (or exported raw ""))))
              (with-temp-buffer
                (insert s)
                (goto-char (point-min))
                ;; take only first MAX-LINES lines
                (let ((lines (split-string (buffer-string) "\n")))
                  (setq lines (seq-take lines max-lines))
                  (string-join lines "\n"))))))))))

(defun org-annotation-hover--generate-preview (id)
  "Build a textual preview for org id ID by locating the node and extracting content."
  (condition-case err
      (let* ((loc (ignore-errors (org-id-find id t))) ; returns (file . position)
             (file (if (consp loc) (car loc) (and (stringp loc) loc)))
             (pos (if (consp loc) (cdr loc) nil)))
        (if (and file pos)
            (let ((text (org-annotation-hover--extract-subtree-as-plain file pos)))
              (org-annotation-hover--cache-put id text file)
              text)
          ;; fallback: short message
          (let ((msg (format "(annotation not found: %s)" id)))
            (org-annotation-hover--cache-put id msg nil)
            msg)))
    (error
     (let ((msg (format "(preview error: %s)" (error-message-string err))))
       (org-annotation-hover--cache-put id msg nil)
       msg))))

(defun org-annotation-hover--get-preview (id)
  "Return preview string for ID, using cache when possible."
  (or (plist-get (org-annotation-hover--cache-get id) :text)
      (org-annotation-hover--generate-preview id)))

;;; Posframe support (optional)
(defun org-annotation-hover--maybe-show-posframe (id pos)
  "Show posframe for ID at POS if posframe is available and enabled.
POS should be a buffer position or (WINDOW . POSITION) cons suitable to `posframe-show'."
  (when (and org-annotation-hover-use-posframe (require 'posframe nil t))
    (let ((text (org-annotation-hover--get-preview id)))
      (posframe-show " *org-annotation-hover*"
                     :string text
                     :position pos
                     :timeout 8
                     :border-width 1))))

(defun org-annotation-hover--maybe-hide-posframe ()
  "Hide the posframe used by org-annotation-hover (if any)."
  (when (and (require 'posframe nil t)
             (posframe-workable-p))
    (posframe-hide " *org-annotation-hover*")))


;;; Overlay management and scanning

(defun org-annotation-hover--make-overlay (beg end id)
  "Create overlay on region BEG..END representing ID.
The overlay stores id property and sets help-echo to a function that returns the preview."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'org-annotation-hover-id id)
    (overlay-put ov 'mouse-face 'highlight)
    ;; keymap - click to open the id
    (let ((km (make-sparse-keymap)))
      (define-key km [mouse-1]
                  `(lambda ()
                     (interactive)
                     (org-annotation-hover-open-link ,id)))
      (overlay-put ov 'keymap km)

      ;; help-echo: function or string; function will compute the preview lazily
      (overlay-put ov 'help-echo
                   (lambda (&rest _)
                     ;; show posframe in addition to help-echo if requested
                     (when (and org-annotation-hover-use-posframe (fboundp 'posframe-show))
                       (ignore-errors
                         (org-annotation-hover--maybe-show-posframe id (point))))
                     (org-annotation-hover--get-preview id)))
      ov))

  (defun org-annotation-hover--remove-overlays ()
    "Remove overlays installed by org-annotation-hover in this buffer."
    (when (and (listp org-annotation-hover--overlays))
      (dolist (ov org-annotation-hover--overlays)
        (when (overlayp ov) (delete-overlay ov)))
      (setq org-annotation-hover--overlays nil)
      (org-annotation-hover--maybe-hide-posframe)))

  (defun org-annotation-hover--scan-region-and-install (start end)
    "Scan START..END for id: links and install overlays for each.
Matches forms like [[id:UUID]] or [[id:UUID][desc]]."
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              "\\(\\[\\[id:\\([^]\n]+\\)\\]\\)\\(\\[\\([^]\n]+\\)\\]\\)?"
              end t)
        (let* ((beg (match-beginning 1))
               (endm (match-end 1))
               (id (match-string-no-properties 2))
               (ov (org-annotation-hover--make-overlay beg endm id)))
          (push ov org-annotation-hover--overlays)))))

  (defun org-annotation-hover--jit-lock-function (start end _len)
    "Function added to `jit-lock-functions' to install overlays in visible region."
    ;; Clear overlay list and reinstall for region. Simpler approach:
    (org-annotation-hover--remove-overlays)
    ;; install overlays for the visible portion of the buffer
    (let ((s (or start (point-min)))
          (e (or end (point-max))))
      (org-annotation-hover--scan-region-and-install s e)))

;;; Commands: open and node-find wrapper

  (defun org-annotation-hover-open-link (id)
    "Open the org node referenced by ID.
If org-roam is available, prefer org-roam-node-from-id -> org-roam-node-visit."
    (interactive "sID: ")
    (cond
     ((and (featurep 'org-roam)
           (fboundp 'org-roam-node-from-id)
           (fboundp 'org-roam-node-visit))
      (let ((node (ignore-errors (org-roam-node-from-id id))))
        (if node
            (org-roam-node-visit node)
          (org-id-goto id))))
     (t
      (org-id-goto id))))

  (defun my/org-roam--nodes-no-annotations ()
    "Return org-roam nodes excluding those tagged with `org-annotation-hover-annotation-tag'."
    (if (not (featurep 'org-roam))
        (user-error "org-roam not loaded")
      (seq-remove (lambda (node)
                    (member org-annotation-hover-annotation-tag (org-roam-node-tags node)))
                  (org-roam-node-list))))

  (defun my/org-roam-node-find-no-annotations (&optional initial)
    "Like `org-roam-node-find' but exclude annotation nodes."
    (interactive)
    (let* ((nodes (my/org-roam--nodes-no-annotations))
           (choices (mapcar (lambda (n) (cons (org-roam-node-title n) n)) nodes))
           (sel (assoc (completing-read "Node: " (mapcar #'car choices) nil t initial) choices)))
      (when sel (org-roam-node-visit (cdr sel)))))

;;; Minor mode definition

;;;###autoload
  (define-minor-mode org-annotation-hover-mode
    "Buffer-local minor mode to show hover previews for org id: links."
    :lighter " ann-hover"
    :group 'org-annotation-hover
    (if org-annotation-hover-mode
        (progn
          ;; install initial overlays for the visible region and register jit-lock
          (jit-lock-register #'org-annotation-hover--jit-lock-function)
          ;; add a buffer-local after-save hook to invalidate cache for this file
          (add-hook 'after-save-hook
                    (lambda ()
                      (org-annotation-hover--clear-cache (buffer-file-name)))
                    nil t))
      ;; disable
      (jit-lock-unregister #'org-annotation-hover--jit-lock-function)
      (org-annotation-hover--remove-overlays)))

;;; Global enable helper (optional)
;;;###autoload
  (define-globalized-minor-mode org-annotation-hover-global-mode
    org-annotation-hover-mode
    (lambda () (when (derived-mode-p 'org-mode) (org-annotation-hover-mode 1))))

;;; File watcher (optional)
  (defun org-annotation-hover--enable-file-watcher (dir)
    "Enable file-notify watch on DIR to clear cache on changes."
    (when (and (functionp 'file-notify-add-watch) (null org-annotation-hover--file-watch-descriptor))
      (setq org-annotation-hover--file-watch-descriptor
            (file-notify-add-watch (expand-file-name dir)
                                   '(change attribute-change)
                                   (lambda (event)
                                     ;; event = (descriptor action file)
                                     (let ((action (nth 1 event))
                                           (file (nth 2 event)))
                                       (when (memq action '(created changed deleted attribute-changed))
                                         (org-annotation-hover--clear-cache file)))))))

    (defun org-annotation-hover--disable-file-watcher ()
      "Disable the file-notify watcher if active."
      (when org-annotation-hover--file-watch-descriptor
        (file-notify-rm-watch org-annotation-hover--file-watch-descriptor)
        (setq org-annotation-hover--file-watch-descriptor nil)))

    (when org-annotation-hover-enable-filewatch
      (condition-case _
          (org-annotation-hover--enable-file-watcher (or (and (boundp 'org-roam-directory) org-roam-directory)
                                                         (and (boundp 'org-directory) org-directory)
                                                         "~"))
        (error nil)))

;;; Public utilities

    (defun org-annotation-hover-show-preview-at-point ()
      "Show the preview for any id: link at point in a posframe (if available) or echo area."
      (interactive)
      (let ((elt (org-element-context)))
        (if (and (eq (org-element-type elt) 'link)
                 (string= (org-element-property :type elt) "id"))
            (let ((id (org-element-property :path elt)))
              (if org-annotation-hover-use-posframe
                  (org-annotation-hover--maybe-show-posframe id (point))
                (message "%s" (org-annotation-hover--get-preview id))))
          (user-error "No id: link at point"))))

;;; Capture template helper (helper var you can add to org-roam-capture-templates)
    (defconst org-annotation-hover-capture-template
      '("a" "annotation" plain
        "%?"
        :target (file+head "annotations/${slug}.org" "#+title: ${title}\n#+filetags: :annotation:\n")
        :unnarrowed t)
      "A capture template you can add to `org-roam-capture-templates' to create annotation nodes.")

;;; Keybinding helper: open id at point
    (defun org-annotation-hover-open-link-at-point ()
      "Open the id: link at point (if any)."
      (interactive)
      (let ((elt (org-element-context)))
        (if (and (eq (org-element-type elt) 'link)
                 (string= (org-element-property :type elt) "id"))
            (org-annotation-hover-open-link (org-element-property :path elt))
          (user-error "No id: link at point"))))

;;; Simple convenience commands
;;;###autoload
    (defun org-annotation-hover-clear-all-cache ()
      "Clear the entire annotation preview cache."
      (interactive)
      (org-annotation-hover--clear-cache)
      (message "org-annotation-hover: cleared cache"))

;;; Provide
    (provide 'org-annotation-hover)
;;; org-annotation-hover.el ends here
