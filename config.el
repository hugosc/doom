(setq org-roam-directory "~/Documents/brain2")
(setq org-directory "~/Documents/brain2/")
(require 'server)
(unless (server-running-p)
  (server-start))

;; Also ensure server starts if running in daemon mode
(when (daemonp)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (when (not (server-running-p))
        (server-start)))))
(global-auto-revert-mode 1)
(setq auto-revert-verbose t)
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq display-line-numbers-type t)
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(use-package! ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "doom-one"))

(use-package! ewal-doom-themes
  :after ewal
  :config (progn
            (load-theme 'ewal-doom-one t)
            (enable-theme 'ewal-doom-one)))
(defun reload-pywal-theme ()
  "Reload the pywal theme to reflect new colors."
  (interactive)
  (ewal-load-colors)
  (load-theme 'ewal-doom-one t)
  (message "Pywal theme reloaded!"))
;; Toggle emacs frame alpha (transparency) to match kitty
(defun toggle-emacs-transparency ()
  "Toggle emacs frame transparency between opaque (100%) and semi-transparent (85%)."
  (interactive)
  (let* ((alpha-param (frame-parameter nil 'alpha-background))
         (current-alpha (or alpha-param 100))
         (new-alpha (if (>= current-alpha 95) 85 100)))
    (set-frame-parameter nil 'alpha-background new-alpha)
    (message "Emacs transparency: %d%%" new-alpha)))

;; Watch for changes to pywal colors and reload theme automatically
(defun my/watch-pywal-colors ()
  "Set up file watcher for pywal colors.json to auto-reload theme."
  (when (functionp 'file-notify-add-watch)
    (let ((colors-file (expand-file-name "~/.cache/wal/colorscheme.json")))
      (when (file-exists-p colors-file)
        (file-notify-add-watch colors-file '(change)
          (lambda (event)
            (when (eq (nth 1 event) 'changed)
              (reload-pywal-theme))))))))

;; Auto-enable the color watcher when emacs starts
(my/watch-pywal-colors)

;; Set initial transparency to match kitty (85%)
(set-frame-parameter nil 'alpha-background 85)
(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))
(after! org-roam
  (setq org-roam-v2-ack t)
  (setq org-roam-capture-templates
        '(("l" "Lecture quicknote" plain
           "%?\n* Post-Lecture Checklist\n- [ ] Extract definitions\n- [ ] Create/update concept node\n- [ ] Link readings\n- [ ] Mark processed"
           :if-new (file+head "academic/lectures/raw/%<%Y-%m-%d>-${slug}.org"
                              "#+TITLE: %<%Y-%m-%d> ${title} Lecture\n#+FILETAGS: :lecture:raw:\n")
           :unnarrowed t)
          ("c" "Concept node" plain
           "* Overview\n%?\n* Context\n* Relations\n* Applications\n* References"
           :if-new (file+head "concepts/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :concept:seed:\n")
           :unnarrowed t)
          ("r" "Reading note" plain
           "* Summary\n%?\n* Key Claims\n* Methods\n* Critical Reflections\n* Links"
           :if-new (file+head "readings/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :reading:seed:\n:PROPERTIES:\n:ROAM_REFS: ${ref}\n:END:\n")
           :unnarrowed t)
          ("a" "Assignment plan" plain
           "* Brief\n%?\n* Requirements\n* Sources\n* Outline\n* Timeline\n* Status"
           :if-new (file+head "assignments/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :assignment:active:\n")
           :unnarrowed t)
          ("p" "Project node" plain
           "* Overview\n%?\n* Goals\n* Links\n* Decisions\n* TODO\n* Resources"
           :if-new (file+head "projects/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :project:active:\n")
           :unnarrowed t)
          ("i" "Inbox idea" plain
           "Context:\n%?\nNext Action:"
           :if-new (file+head "inbox.org" "#+TITLE: Inbox\n#+FILETAGS: :inbox:\n")
           :empty-lines-before 1
           :unnarrowed t)
          ("j" "Journal entry" plain
           "%?"
           :if-new (file+head "journal/%<%Y>.org" "#+TITLE: Journal %<%Y>\n#+FILETAGS: :journal:\n")
           :empty-lines-before 1
           :unnarrowed t)))
(defun my/org-roam-promote-heading-to-concept ()
    "Promote current heading into a new concept node.

This:
- Creates `concepts/<slug>.org` (if missing) with a scaffold that uses '* Overview'.
- Copies the current heading's subtree (content below the heading line) into
  the new file under '* Overview' (appends).
- Transfers the original heading's ID to the new file (if it exists), so existing
  links to that heading automatically resolve to the new concept file.
- Removes the ID from the original heading to avoid duplicates.
- Inserts a link to the new concept in the original file's '* Relations' section as a bullet point.

It does NOT delete or replace the original heading." 
    (interactive)
    (unless (org-at-heading-p)
      (user-error "Not at a heading"))

    (let* ((title (nth 4 (org-heading-components)))
           (node (org-roam-node-create :title title))
           (slug (org-roam-node-slug node))
           (file (expand-file-name (format "concepts/%s.org" slug) org-roam-directory))
           ;; original subtree bounds
           (orig-beg (save-excursion (org-back-to-heading t) (point)))
           (orig-end (save-excursion (org-end-of-subtree t t)))
           ;; body = text after the heading line up to end of subtree
           (body (save-restriction
                   (widen)
                   (when (> orig-end orig-beg)
                     (save-excursion
                       (goto-char orig-beg)
                       (forward-line 1)
                       ;; Skip properties drawer if present
                       (when (looking-at "^[ \t]*:PROPERTIES:")
                         (when (re-search-forward "^[ \t]*:END:" orig-end t)
                           (forward-line 1)))
                       ;; Skip any blank lines after properties
                       (while (and (< (point) orig-end) (looking-at-p "^\\s-*$"))
                         (forward-line 1))
                       ;; Extract the actual content
                       (let ((content-start (point)))
                         (when (< content-start orig-end)
                           (string-trim (buffer-substring-no-properties content-start orig-end)))))))))
      ;; Ensure original heading has an ID so we can link to it later if needed
      (let ((orig-id (save-excursion
                       (org-back-to-heading t)
                       (or (org-entry-get nil "ID")
                           (org-id-get-create)))))
        ;; Create scaffold if file doesn't exist
        (unless (file-exists-p file)
          (with-temp-file file
            (insert ":PROPERTIES:\n:END:\n#+TITLE: " title "\n#+FILETAGS: :concept:seed:\n\n* Overview\n\n* Context\n* Relations\n* Applications\n* References\n")))
        ;; Transfer or create ID: use heading's existing ID if present, otherwise generate new one
        (let ((new-id (if orig-id
                          ;; Use the original heading's ID for the new file
                          (progn
                            (with-current-buffer (find-file-noselect file)
                              (goto-char (point-min))
                              (when (re-search-forward "^:PROPERTIES:" nil t)
                                (forward-line 1)
                                (insert (format ":ID: %s\n" orig-id)))
                              (save-buffer))
                            ;; Delete entire properties drawer from original heading
                            (save-excursion
                              (org-back-to-heading t)
                              (when (re-search-forward "^[ \t]*:PROPERTIES:" (save-excursion (outline-next-heading) (point)) t)
                                (let ((beg (line-beginning-position)))
                                  (when (re-search-forward "^[ \t]*:END:" (save-excursion (outline-next-heading) (point)) t)
                                    (delete-region beg (1+ (line-end-position)))))))
                            (save-buffer)
                            orig-id)
                        ;; No existing ID, generate a new one for the concept file
                        (let ((generated-id (org-id-new)))
                          (with-current-buffer (find-file-noselect file)
                            (goto-char (point-min))
                            (when (re-search-forward "^:PROPERTIES:" nil t)
                              (forward-line 1)
                              (insert (format ":ID: %s\n" generated-id)))
                            (save-buffer))
                          generated-id))))
          ;; Insert body into Overview if present
          (when (and body (not (string-empty-p (string-trim body))))
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^\\* Overview\\s-*$" nil t)
                  (forward-line 1)
                  ;; Clean up: remove any existing blank lines after Overview heading
                  (while (looking-at-p "^\\s-*$")
                    (delete-region (line-beginning-position) (1+ (line-end-position))))
                  ;; Insert the trimmed body content
                  (insert (string-trim body) "\n")
                  (save-buffer)))))

          ;; Insert a link to the NEW concept into the original file's *Relations section
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "^\\* Relations\\s-*$" nil t)
                ;; Found Relations section, insert link
                (progn
                  (forward-line 1)
                  (insert (format "- [[id:%s][%s]]\n" new-id title))
                  (save-buffer))
              ;; No Relations section, create one at end of file
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (format "* Relations\n- [[id:%s][%s]]\n" new-id title))
              (save-buffer)))
          (message "Promoted '%s' → %s (id:%s) and inserted link in original file" title file new-id)))))
(defun my/insert-file-link (path)
  "Insert an org file link to PATH with basename as description."
  (interactive "fFile: ")
  (insert (format "[[file:%s][%s]]" (abbreviate-file-name path) (file-name-base path))))
(defun my/add-heading-id ()
  "Add org-id UUID to heading with filename-heading alias for org-roam."
  (interactive)
  (org-back-to-heading t)
  (let* ((filename (file-name-base (buffer-file-name)))
         (title (nth 4 (org-heading-components)))
         (alias (format "%s - %s" filename title))
         (id (org-id-get-create)))
    (org-set-property "ROAM_ALIASES" (format "\"%s\"" alias))
    (message "Added ID with alias: %s" alias)))
(defun my/insert-heading-link ()
  "Pick an org file recursively, then link to a heading with org-id."
  (interactive)
  (let* ((current-buf (current-buffer))
         (brain2-dir (expand-file-name "~/Documents/brain2/"))
         (default-directory brain2-dir)
         ;; Get all org files recursively
         (org-files (directory-files-recursively brain2-dir "\\.org$"))
         ;; Make paths relative for cleaner display
         (file-choices (mapcar (lambda (f) (file-relative-name f brain2-dir)) org-files))
         (file-rel (completing-read "Org file: " file-choices nil t))
         (file (expand-file-name file-rel brain2-dir))
         (filename (file-name-base file)))
    (unless (file-exists-p file)
      (user-error "File not found: %s" file))
    ;; List headings in that file
    (with-current-buffer (find-file-noselect file)
      (let* ((headings (org-map-entries (lambda () (nth 4 (org-heading-components))) nil 'file))
             (heading (completing-read "Heading: " headings)))
        ;; Find the heading and get/create its ID
        (goto-char (point-min))
        (unless (search-forward heading nil t)
          (user-error "Heading not found: %s" heading))
        (org-back-to-heading t)
        (let* ((id (org-id-get-create))
               (alias (format "%s - %s" filename heading))
               (link (format "[[id:%s][%s]]" id heading)))
          ;; Ensure alias is set
          (unless (org-entry-get nil "ROAM_ALIASES")
            (org-set-property "ROAM_ALIASES" (format "\"%s\"" alias)))
          ;; Switch back to original buffer and insert link
          (with-current-buffer current-buf
            (insert link)
            (message "Inserted link: %s" link)))))))
(map! :leader
      :desc "Promote heading to concept" "n P" #'my/org-roam-promote-heading-to-concept
      :desc "Insert file link" "n f" #'my/insert-file-link
      :desc "Add heading ID" "n i" #'my/add-heading-id
      :desc "Insert heading link" "n h" #'my/insert-heading-link
      :desc "Find/create node" "n n" #'org-roam-node-find
      :desc "Insert node link" "n l" #'org-roam-node-insert
      :desc "Toggle backlinks" "n b" #'org-roam-buffer-toggle
      :desc "Open graph UI" "n g" #'org-roam-ui-open
      :desc "Capture new node" "n c" #'org-roam-capture)
)
(defun my/mark-lecture-processed ()
  "Mark current lecture note as processed and move to processed directory."
  (interactive)
  (let* ((file (buffer-file-name))
         (rel (file-relative-name file org-roam-directory)))
    (unless (string-match-p "academic/lectures/raw/" rel)
      (user-error "Not in raw lectures directory"))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "POST-LECTURE CHECKLIST" nil t)
        (goto-char (point-max)))
      (save-buffer)
      (org-set-property "PROCESSED" (format-time-string "%Y-%m-%d")))
    (let* ((dest (expand-file-name (concat "academic/lectures/processed/" (file-name-nondirectory file)) org-roam-directory)))
      (rename-file file dest 1)
      (find-file dest)
      (message "Lecture processed & moved."))))

(map! :leader :desc "Process lecture" "n L" #'my/mark-lecture-processed)
(map! :leader :desc "Find file (live)" "SPC" #'consult-fd)
(use-package! gptel
  :config
  (setq gptel-model 'gpt-5-mini
        gptel-backend (gptel-make-gh-copilot "Copilot")))
(use-package! presence
  :defer t
  :commands (presence-mode)
  :init
  (add-hook 'doom-first-buffer-hook #'presence-mode)
  :config
  ;; Use custom Discord application
  (setq presence-client-id "1443438985878962228")
  ;; Use asset keys directly (not URLs) for our custom Discord app
  (setq presence-icon-base nil)
  ;; Override to return just the asset key
  (defun presence--resolve-icon-base (icon)
    "Return just the icon asset key for Discord."
    icon)
  ;; Use custom icon from our Discord app
  (setq presence-editor-icon "emacs-icon")
  ;; Keep editor as main icon, mode as small icon
  (setq presence-use-major-mode-as-main-icon nil)
  ;; Show the small icon (mode icon)
  (setq presence-show-small-icon t)
  ;; Add extra mode icon mappings
  (setq presence-mode-icon-alist
        (append '((magit-status-mode . "git")
                  (magit-log-mode . "git")
                  (magit-diff-mode . "git")
                  (magit-revision-mode . "git")
                  (conf-toml-mode . "toml"))
                presence-mode-icon-alist))
  ;; Hide line numbers from status
  (setq presence-display-line-numbers nil)
  ;; Custom format: show project name + file instead of just buffer name
  (defun my/presence-buffer-details ()
    (let ((project (projectile-project-name))
          (file (buffer-name)))
      (if (and project (not (string= project "-")))
          (format "In %s: %s" project file)
        (format "Editing %s" file))))
  (setq presence-buffer-details-format-function #'my/presence-buffer-details))
(after! org
  (setq org-agenda-files '("~/Documents/brain2/")
        org-hide-emphasis-markers t)
  ;; Fallback: ensure emphasis markers are hidden when org-mode starts
  (add-hook 'org-mode-hook (lambda () (setq org-hide-emphasis-markers t))))
(after! org
  (defun my/org-format-buffer ()
    "Format org buffer with consistent spacing."
    (interactive)
    (message "Running org-format-buffer...")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (let ((heading-pos (line-beginning-position)))
          ;; Check if next line is properties drawer
          (forward-line 1)
          (when (looking-at "^[ \t]*:PROPERTIES:")
            ;; Remove any blank lines between heading and properties
            (goto-char heading-pos)
            (forward-line 1)
            (while (and (not (looking-at "^[ \t]*:PROPERTIES:"))
                       (looking-at-p "^\\s-*$"))
              (delete-region (line-beginning-position) (1+ (line-end-position))))
            ;; Find end of properties
            (when (re-search-forward "^[ \t]*:END:" nil t)
              (forward-line 1)
              ;; Ensure exactly one blank line after properties
              (let ((content-start (point)))
                (while (looking-at-p "^\\s-*$")
                  (forward-line 1))
                (unless (= (point) (1+ content-start))
                  (delete-region content-start (point))
                  (goto-char content-start)
                  (unless (or (looking-at "^\\*") (eobp))
                    (insert "\n"))))))
          ;; If no properties, ensure blank line after heading
          (goto-char heading-pos)
          (forward-line 1)
          (unless (or (looking-at "^[ \t]*:PROPERTIES:")
                     (looking-at-p "^\\s-*$")
                     (looking-at "^\\*")
                     (eobp))
            (insert "\n")))))
    
    ;; Ensure blank line before each heading (after content)
    (save-excursion
      (goto-char (point-min))
      ;; Skip first heading
      (when (re-search-forward "^\\*+ " nil t)
        (beginning-of-line)
        (while (re-search-forward "^\\*+ " nil t)
          (beginning-of-line)
          (let ((heading-pos (point)))
            (forward-line -1)
            ;; If previous line is not blank and not a heading, add blank line
            (when (and (not (looking-at-p "^\\s-*$"))
                       (not (looking-at "^\\*")))
              (end-of-line)
              (insert "\n"))
            ;; Move past this heading to continue loop
            (goto-char heading-pos)
            (forward-line 1))))))
  
   ;; Run formatting on save
   (add-hook 'org-mode-hook
     (lambda ()
       (add-hook 'before-save-hook #'my/org-format-buffer nil t))))
(setq +zen-text-scale 1)           ;; Font size scale (1 = one size larger)
(setq writeroom-width 70)          ;; Column width (narrower = wider margins)

;; Configure org-modern to always be on for org-mode
(after! org-modern
  (setq org-modern-hide-stars 'leading)
  ;; Enable org-modern globally
  (add-hook 'org-mode-hook #'org-modern-mode))

;; Zen mode hook - hide line numbers, adjust org-modern star display
(add-hook 'writeroom-mode-hook
  (lambda ()
    (if writeroom-mode
        (progn
          (display-line-numbers-mode -1)
          (when (derived-mode-p 'org-mode)
            (setq-local org-modern-hide-stars t))
          (text-scale-set 1))
      (progn
        ;; Restore when exiting zen mode
        (display-line-numbers-mode 1)
        (when (derived-mode-p 'org-mode)
          (setq-local org-modern-hide-stars 'leading))))))
;; Use custom emacs logo image as the dashboard banner with minimal menu.

;; Set BEFORE doom-dashboard loads - use pre-scaled high-quality XPM
(setq +doom-dashboard-banner-file "default.xpm"
      +doom-dashboard-banner-dir (expand-file-name "~/.config/emacs/modules/ui/doom-dashboard/banners/"))

(setq +doom-dashboard-menu-sections
      '(("Open project"
         :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Open documentation"
         :icon (nerd-icons-octicon "nf-oct-book" :face 'doom-dashboard-menu-title)
         :action doom/help)))

(after! doom-dashboard

  ;; Adjust banner padding for better vertical centering
  (setq +doom-dashboard-banner-padding '(8 . 4))

  ;; Override the banner widget to properly center the image
  (defun doom-dashboard-widget-banner ()
    (let ((point (point)))
      (if (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
          (let* ((image (create-image (fancy-splash-image-file)))
                 (img-width (car (image-size image t)))
                 ;; Add 2 extra spaces to shift right slightly
                 (line (make-string (max 1 (+ img-width 2)) ? )))
            ;; Insert a centered spacer line and display the image on it
            (insert (+doom-dashboard--center +doom-dashboard--width line) "\n")
            (add-text-properties
             point (1- (point)) `(display ,image rear-nonsticky (display)))
            (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0)
                                 ?\n)))
        ;; Fallback to ASCII banner in terminal mode (optional)
        nil)))

  ;; Custom shortmenu with tighter spacing - use single newline instead of double
  (defun my/doom-dashboard-widget-shortmenu ()
    (insert "\n")
    (dolist (section +doom-dashboard-menu-sections)
      (cl-destructuring-bind (label &key icon action when face key) section
        (when (and (fboundp action)
                   (or (null when)
                       (eval when t)))
          (insert
           (+doom-dashboard--center
            (- +doom-dashboard--width 1)
            (let ((icon (if (stringp icon) icon (eval icon t))))
              (format (format "%s%%s%%-10s" (if icon "%3s\t" "%3s"))
                      (or icon "")
                      (with-temp-buffer
                        (insert-text-button
                         label
                         'action
                         `(lambda (_)
                            (call-interactively (or (command-remapping #',action)
                                                    #',action)))
                         'face (or face 'doom-dashboard-menu-title)
                         'follow-link t
                         'help-echo
                         (format "%s (%s)" label
                                 (propertize (symbol-name action) 'face 'doom-dashboard-menu-desc)))
                        (format "%-37s" (buffer-string)))
                      ;; Lookup command keys dynamically
                      (propertize
                       (or key
                           (when-let*
                               ((keymaps
                                 (delq
                                  nil (list (when (bound-and-true-p evil-local-mode)
                                              (evil-get-auxiliary-keymap +doom-dashboard-mode-map 'normal))
                                            +doom-dashboard-mode-map)))
                                (key
                                 (or (when keymaps
                                       (where-is-internal action keymaps t))
                                     (where-is-internal action nil t))))
                             (with-temp-buffer
                               (save-excursion (insert (key-description key)))
                               (while (re-search-forward "<\\([^>]+\\)>" nil t)
                                 (let ((str (match-string 1)))
                                   (replace-match
                                    (upcase (if (< (length str) 3)
                                                str
                                              (substring str 0 3))))))
                               (buffer-string)))
                           "")
                       'face 'doom-dashboard-menu-desc))))
           "\n")))))  ;; Use single \n instead of \n\n

  ;; Override dashboard widgets - remove loaded time, keep banner and menu
  (setq +doom-dashboard-functions
        '(doom-dashboard-widget-banner
          my/doom-dashboard-widget-shortmenu
          doom-dashboard-widget-footer))

  ;; Force reload dashboard to apply changes
  (when (get-buffer +doom-dashboard-name)
    (+doom-dashboard-reload t)))
(after! vterm
  (add-hook 'vterm-mode-hook
    (lambda ()
      ;; ESC sends to terminal, not evil
      (evil-define-key 'insert vterm-mode-map
        (kbd "<escape>") 'vterm-send-escape)
      ;; Ctrl-c Ctrl-c enters normal mode
      (evil-define-key 'insert vterm-mode-map
        (kbd "C-c C-c") 'evil-normal-state))))
