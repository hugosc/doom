(setq org-roam-directory "~/Documents/brain2")
(setq org-directory "~/Documents/brain2/")
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
(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-hide-emphasis-markers t
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
(after! org
  (setq org-agenda-files '("~/Documents/brain2/")))
