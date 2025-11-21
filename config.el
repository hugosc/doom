;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Org-Roam directory (must be set before org-roam loads)
(setq org-roam-directory "~/Documents/brain2")

;; Auto-revert files when they change externally
;; This allows OpenCode to make changes and have them display automatically
(global-auto-revert-mode 1)
(setq auto-revert-verbose t)  ;; Shows a message when reverting

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:


;; Org Roam UI Settings
(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-hide-emphasis-markers t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; Pywal theme integration
(use-package! ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "doom-one"))

(use-package! ewal-doom-themes
  :after ewal
  :config (progn
            (load-theme 'ewal-doom-one t)
            (enable-theme 'ewal-doom-one)))


;; Uncomment the line below if you want to use a different ewal theme:
;; Options: ewal-doom-one, ewal-doom-vibrant, ewal-doom-dark
;; (setq doom-theme 'ewal-doom-one)

;; Helper function to reload pywal colors
;; Use M-x reload-pywal-theme after changing wallpaper
(defun reload-pywal-theme ()
  "Reload the pywal theme to reflect new colors."
  (interactive)
  (ewal-load-colors)
  (load-theme 'ewal-doom-one t)
  (message "Pywal theme reloaded!"))

;; Optional: bind to a key
;; (map! :leader
;;       :desc "Reload pywal theme" "t w" #'reload-pywal-theme)

;; Optional: Fine-tune consult-fd behavior
;; (after! consult
;;   (setq consult-fd-args '("fd" "--color=never" "--full-path")))

;; -----------------------------
;; Org-roam Capture Framework
;; -----------------------------
(after! org-roam
  (setq org-roam-v2-ack t)
  (setq org-roam-capture-templates
        '(("l" "Lecture quicknote" plain
           "%?\n* Post-Lecture Checklist\n- [ ] Extract definitions\n- [ ] Create/update concept node\n- [ ] Link readings\n- [ ] Mark processed"
           :if-new (file+head "academic/lectures/raw/%<%Y-%m-%d>-${slug}.org"
                              "#+TITLE: %<%Y-%m-%d> ${title} Lecture\n#+FILETAGS: :lecture:raw:\n")
           :unnarrowed t)
          ("c" "Concept node" plain
           "* Definition\n%?\n* Context\n* Relations\n* Applications\n* References"
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

  ;; Promote a heading in inbox or lecture file to a concept node
  (defun my/org-roam-promote-heading-to-concept ()
    "Convert current heading into a new concept node, replacing it with a link."
    (interactive)
    (unless (org-at-heading-p) (user-error "Not at a heading"))
    (let* ((title (nth 4 (org-heading-components)))
           (slug (org-roam-node-slug (org-roam-node-create :title title)))
           (file (expand-file-name (format "concepts/%s.org" slug) org-roam-directory))
           (id (org-id-get-create)))
      (unless (file-exists-p file)
        (with-temp-file file
          (insert (format "#+TITLE: %s\n#+FILETAGS: :concept:seed:\n* Definition\n%s\n* Context\n* Relations\n* Applications\n* References\n" title ""))))
      (let ((new-id (with-current-buffer (find-file-noselect file)
                      (goto-char (point-min))
                      (org-id-get-create))))
        (org-back-to-heading t)
        (let ((beg (point))
              (end (save-excursion (org-end-of-subtree t t))))
          (delete-region beg end)
          (insert (format "[[id:%s][%s]]" new-id title))))
      (message "Promoted heading '%s' to concept node." title)))

  (map! :leader
        :desc "Promote heading to concept" "n P" #'my/org-roam-promote-heading-to-concept)

  ;; Insert file link to external project resource
  (defun my/insert-file-link (path)
    "Insert an org file link to PATH with basename as description."
    (interactive "fFile: ")
    (insert (format "[[file:%s][%s]]" (abbreviate-file-name path) (file-name-base path))))
  (map! :leader :desc "Insert file link" "n f" #'my/insert-file-link)

  ;; Add UUID ID to heading and set alias as "filename - heading"
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
  (map! :leader :desc "Add heading ID" "n i" #'my/add-heading-id)

  ;; Insert link to a heading (with UUID ID)
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
              (message "Inserted link: %s" link))))))))
  (map! :leader :desc "Insert heading link" "n h" #'my/insert-heading-link)

  ;; Processing helper: mark lecture processed and move file
  (defun my/mark-lecture-processed ()
    "Mark current lecture note as processed and move to processed directory."\n    (interactive)
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

  ;; Standard org-roam keybindings (leader prefix)
  (map! :leader
        :desc "Find/create node" "n n" #'org-roam-node-find
        :desc "Insert node link" "n l" #'org-roam-node-insert
        :desc "Toggle backlinks" "n b" #'org-roam-buffer-toggle
        :desc "Open graph UI" "n g" #'org-roam-ui-open
        :desc "Capture new node" "n c" #'org-roam-capture)

  ;; Live file picker: SPC SPC uses consult-fd (no caching, Telescope-like)
  (map! :leader :desc "Find file (live)" "SPC" #'consult-fd)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/brain2/org-roam/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
