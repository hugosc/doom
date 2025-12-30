;;; ewal-fix.el --- Fix ewal color shading for light themes -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Crocodile
;; Keywords: faces, themes

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This fixes ewal's color shading algorithm which incorrectly blends
;; towards hardcoded white/black instead of towards foreground/background.
;;
;; Original ewal behavior (BROKEN for light themes):
;; - Positive shade: blend towards #FFFFFF (white)
;; - Negative shade: blend towards #000000 (black)
;;
;; Result for light themes:
;; - base4 (line numbers) = light bg blended with white = nearly white (wrong!)
;;
;; Fixed behavior:
;; - Positive shade: blend towards foreground color
;; - Negative shade: blend towards background color
;;
;; Result for light themes:
;; - base4 = light bg blended towards dark fg = grey (correct!)
;;
;; Result for dark themes:
;; - base4 = dark bg blended towards light fg = grey (correct!)

;;; Code:

(require 'ewal)
(require 'color)

(defun ewal--color-chshade-fixed (color alpha)
  "Change shade of COLOR by ALPHA, blending towards foreground/background.
This fixes the original ewal implementation which always blends towards white/black.

For positive ALPHA: blend towards foreground (makes colors closer to text color)
For negative ALPHA: blend towards background (makes colors closer to bg color)

This works correctly for both light and dark themes because it uses
the actual foreground/background colors instead of hardcoded white/black."
  (cond ((and color (symbolp color))
         (ewal--color-chshade-fixed (ewal--get-base-color color) alpha))
        ((listp color)
         (cl-loop for c in color collect (ewal--color-chshade-fixed c alpha)))
        (t
         (if (> alpha 0)
             ;; Positive shade: blend towards foreground
             (ewal--color-blend (ewal--get-base-color 'foreground) color alpha)
           ;; Negative shade: blend towards background
           (ewal--color-blend (ewal--get-base-color 'background) color (* -1 alpha))))))

;; Override the original broken function
(advice-add 'ewal--color-chshade :override #'ewal--color-chshade-fixed)

;; Comprehensive face definitions for ewal-doom-one theme
;; This provides first-class theme coverage matching doom-gruvbox quality
;; while using pywal-generated colors dynamically (adapts to any wallpaper)
(defun ewal-fix-comprehensive-faces ()
  "Apply comprehensive face definitions to ewal-doom-one theme.
Provides first-class coverage for org-mode, magit, dired, which-key, evil, and more.
Only applies to ewal-doom-one theme (wallpaper-based), not preset themes.

Uses pywal colors dynamically by extracting them from current font-lock faces,
so it adapts automatically to any wallpaper's color scheme."
  (when (member 'ewal-doom-one custom-enabled-themes)
    ;; Extract colors dynamically from current theme
    ;; These will change with each wallpaper, so we reference faces, not hex values
    (let* ((bg         (or (face-background 'default nil t) "#000000"))
           (fg         (or (face-foreground 'default nil t) "#ffffff"))
           (keyword    (or (face-foreground 'font-lock-keyword-face nil t) fg))
           (string     (or (face-foreground 'font-lock-string-face nil t) fg))
           (function   (or (face-foreground 'font-lock-function-name-face nil t) fg))
           (variable   (or (face-foreground 'font-lock-variable-name-face nil t) fg))
           (type       (or (face-foreground 'font-lock-type-face nil t) fg))
           (constant   (or (face-foreground 'font-lock-constant-face nil t) fg))
           (builtin    (or (face-foreground 'font-lock-builtin-face nil t) fg))
           (comment    (or (face-foreground 'font-lock-comment-face nil t) fg))
           (cursor-col (or (face-background 'cursor nil t) fg))
           ;; Derive additional colors using doom-themes functions
           (bg-light   (doom-lighten bg 0.05))
           (bg-lighter (doom-lighten bg 0.1))
           (selection  (doom-lighten bg 0.15))
           (fg-dim     (doom-darken fg 0.3))
           (keyword-bright (doom-lighten keyword 0.2))
           (string-dim (doom-darken string 0.2)))
      
      ;; ========================================================================
      ;; BASE UI ELEMENTS
      ;; ========================================================================
      
      (set-face-attribute 'button nil
                          :foreground keyword :underline t :weight 'bold)
      (set-face-attribute 'cursor nil
                          :background cursor-col)
      (set-face-attribute 'hl-line nil
                          :background bg-light)
      (set-face-attribute 'region nil
                          :background selection :distant-foreground fg)
      (set-face-attribute 'isearch nil
                          :foreground bg :background function :weight 'bold)
      (set-face-attribute 'lazy-highlight nil
                          :foreground bg :background keyword :weight 'bold)
      (set-face-attribute 'link nil
                          :foreground keyword :underline t)
      (set-face-attribute 'minibuffer-prompt nil
                          :foreground function :weight 'bold)
      
      ;; Line numbers (subtle - blend toward background for less prominence)
      (set-face-attribute 'line-number nil
                          :foreground (doom-blend comment bg 0.3))
      (set-face-attribute 'line-number-current-line nil
                          :foreground fg :weight 'bold)
      
      ;; Show matching parens
      (set-face-attribute 'show-paren-match nil
                          :foreground 'unspecified :background bg-lighter :weight 'bold)
      (set-face-attribute 'show-paren-mismatch nil
                          :foreground fg :background string :weight 'bold)
      
      ;; Modeline - ensure foreground is visible
      (set-face-attribute 'mode-line nil
                          :foreground fg :background bg-lighter)
      (set-face-attribute 'mode-line-inactive nil
                          :foreground comment :background bg-light)
      
      ;; Doom-modeline specific faces - prevent dark text on dark background
      (when (facep 'doom-modeline-buffer-path)
        (set-face-attribute 'doom-modeline-buffer-path nil
                            :foreground keyword :weight 'bold))
      (when (facep 'doom-modeline-buffer-file)
        (set-face-attribute 'doom-modeline-buffer-file nil
                            :foreground fg :weight 'bold))
      (when (facep 'doom-modeline-project-dir)
        (set-face-attribute 'doom-modeline-project-dir nil
                            :foreground function :weight 'bold))
      (when (facep 'doom-modeline-project-root-dir)
        (set-face-attribute 'doom-modeline-project-root-dir nil
                            :foreground function :weight 'bold))
      (when (facep 'doom-modeline-project-name)
        (set-face-attribute 'doom-modeline-project-name nil
                            :foreground function :weight 'bold))
      (when (facep 'doom-modeline-project-parent-dir)
        (set-face-attribute 'doom-modeline-project-parent-dir nil
                            :foreground keyword :weight 'bold))
      (when (facep 'doom-modeline-buffer-modified)
        (set-face-attribute 'doom-modeline-buffer-modified nil
                            :foreground constant :weight 'bold))
      (when (facep 'doom-modeline-info)
        (set-face-attribute 'doom-modeline-info nil
                            :foreground keyword :weight 'bold))
      (when (facep 'doom-modeline-warning)
        (set-face-attribute 'doom-modeline-warning nil
                            :foreground constant :weight 'bold))
      (when (facep 'doom-modeline-urgent)
        (set-face-attribute 'doom-modeline-urgent nil
                            :foreground string :weight 'bold))
      (when (facep 'doom-modeline-bar)
        (set-face-attribute 'doom-modeline-bar nil
                            :background keyword))
      
      ;; Git branch (vcs) faces
      (when (facep 'doom-modeline-vcs-default)
        (set-face-attribute 'doom-modeline-vcs-default nil
                            :foreground builtin :weight 'bold))
      (when (facep 'doom-modeline-vcs-icon)
        (set-face-attribute 'doom-modeline-vcs-icon nil
                            :foreground builtin))
      
      ;; ========================================================================
      ;; ORG-MODE (your heavy usage)
      ;; ========================================================================
      
      ;; Org document structure
      (when (facep 'org-document-title)
        (set-face-attribute 'org-document-title nil
                            :foreground function :weight 'bold :height 1.05))
      (when (facep 'org-document-info)
        (set-face-attribute 'org-document-info nil
                            :foreground string))
      (when (facep 'org-document-info-keyword)
        (set-face-attribute 'org-document-info-keyword nil
                            :foreground comment))
      
      ;; Org headings - ensure they're visible with good hierarchy
      ;; No large height scaling, just weight and color for hierarchy
      (when (facep 'org-level-1)
        (set-face-attribute 'org-level-1 nil :foreground keyword :weight 'bold :height 1.0))
      (when (facep 'org-level-2)
        (set-face-attribute 'org-level-2 nil :foreground function :weight 'bold :height 1.0))
      (when (facep 'org-level-3)
        (set-face-attribute 'org-level-3 nil :foreground builtin :weight 'bold :height 1.0))
      (when (facep 'org-level-4)
        (set-face-attribute 'org-level-4 nil :foreground variable :weight 'bold :height 1.0))
      (when (facep 'org-level-5)
        (set-face-attribute 'org-level-5 nil :foreground type :height 1.0))
      (when (facep 'org-level-6)
        (set-face-attribute 'org-level-6 nil :foreground string :height 1.0))
      (when (facep 'org-level-7)
        (set-face-attribute 'org-level-7 nil :foreground constant :height 1.0))
      (when (facep 'org-level-8)
        (set-face-attribute 'org-level-8 nil :foreground comment :height 1.0))
      
      ;; Org TODO states
      (when (facep 'org-todo)
        (set-face-attribute 'org-todo nil
                            :foreground string :weight 'bold))
      (when (facep 'org-done)
        (set-face-attribute 'org-done nil
                            :foreground comment :weight 'bold :strike-through t))
      
      ;; Org special elements
      (when (facep 'org-date)
        (set-face-attribute 'org-date nil
                            :foreground type :underline t))
      (when (facep 'org-tag)
        (set-face-attribute 'org-tag nil
                            :foreground comment :weight 'normal))
      (when (facep 'org-code)
        (set-face-attribute 'org-code nil
                            :foreground constant :background bg-light))
      (when (facep 'org-verbatim)
        (set-face-attribute 'org-verbatim nil
                            :foreground string))
      (when (facep 'org-table)
        (set-face-attribute 'org-table nil
                            :foreground keyword))
      (when (facep 'org-link)
        (set-face-attribute 'org-link nil
                            :foreground keyword :underline t))
      (when (facep 'org-drawer)
        (set-face-attribute 'org-drawer nil
                            :foreground comment))
      (when (facep 'org-meta-line)
        (set-face-attribute 'org-meta-line nil
                            :foreground comment))
      (when (facep 'org-block)
        (set-face-attribute 'org-block nil
                            :background bg-light :extend t))
      (when (facep 'org-block-begin-line)
        (set-face-attribute 'org-block-begin-line nil
                            :foreground comment :background bg-light :extend t))
      (when (facep 'org-block-end-line)
        (set-face-attribute 'org-block-end-line nil
                            :foreground comment :background bg-light :extend t))
      (when (facep 'org-quote)
        (set-face-attribute 'org-quote nil
                            :foreground fg-dim :background bg-light :slant 'italic :extend t))
      (when (facep 'org-ellipsis)
        (set-face-attribute 'org-ellipsis nil
                            :foreground comment :underline nil))
      (when (facep 'org-checkbox)
        (set-face-attribute 'org-checkbox nil
                            :foreground keyword :weight 'bold))
      (when (facep 'org-formula)
        (set-face-attribute 'org-formula nil
                            :foreground function))
      
      ;; Org agenda
      (when (facep 'org-agenda-date)
        (set-face-attribute 'org-agenda-date nil :foreground keyword))
      (when (facep 'org-agenda-date-today)
        (set-face-attribute 'org-agenda-date-today nil :foreground function :weight 'bold))
      (when (facep 'org-agenda-structure)
        (set-face-attribute 'org-agenda-structure nil :foreground keyword :weight 'bold))
      
      ;; ========================================================================
      ;; MAGIT (Git interface)
      ;; ========================================================================
      
      (when (facep 'magit-section-heading)
        (set-face-attribute 'magit-section-heading nil
                            :foreground keyword :weight 'bold))
      (when (facep 'magit-section-highlight)
        (set-face-attribute 'magit-section-highlight nil
                            :background bg-light))
      (when (facep 'magit-branch-local)
        (set-face-attribute 'magit-branch-local nil
                            :foreground function))
      (when (facep 'magit-branch-remote)
        (set-face-attribute 'magit-branch-remote nil
                            :foreground builtin))
      (when (facep 'magit-branch-current)
        (set-face-attribute 'magit-branch-current nil
                            :foreground function :weight 'bold :underline t))
      (when (facep 'magit-tag)
        (set-face-attribute 'magit-tag nil
                            :foreground constant))
      
      ;; Magit diff faces - always green for added, red for removed (not theme-dependent)
      (when (facep 'magit-diff-added)
        (set-face-attribute 'magit-diff-added nil
                            :foreground "#22aa22" :background (doom-blend "#22aa22" bg 0.1)))
      (when (facep 'magit-diff-added-highlight)
        (set-face-attribute 'magit-diff-added-highlight nil
                            :foreground "#22aa22" :background (doom-blend "#22aa22" bg 0.2)))
      (when (facep 'magit-diff-removed)
        (set-face-attribute 'magit-diff-removed nil
                            :foreground "#aa2222" :background (doom-blend "#aa2222" bg 0.1)))
      (when (facep 'magit-diff-removed-highlight)
        (set-face-attribute 'magit-diff-removed-highlight nil
                            :foreground "#aa2222" :background (doom-blend "#aa2222" bg 0.2)))
      
      ;; ========================================================================
      ;; DIRED (File manager)
      ;; ========================================================================
      
      (when (facep 'dired-directory)
        (set-face-attribute 'dired-directory nil
                            :foreground keyword :weight 'bold))
      (when (facep 'dired-symlink)
        (set-face-attribute 'dired-symlink nil
                            :foreground builtin :slant 'italic))
      (when (facep 'dired-marked)
        (set-face-attribute 'dired-marked nil
                            :foreground function :weight 'bold))
      (when (facep 'dired-header)
        (set-face-attribute 'dired-header nil
                            :foreground keyword :weight 'bold))
      
      ;; ========================================================================
      ;; WHICH-KEY (Keybinding help)
      ;; ========================================================================
      
      (when (facep 'which-key-key-face)
        (set-face-attribute 'which-key-key-face nil
                            :foreground function :weight 'bold))
      (when (facep 'which-key-command-description-face)
        (set-face-attribute 'which-key-command-description-face nil
                            :foreground fg))
      (when (facep 'which-key-group-description-face)
        (set-face-attribute 'which-key-group-description-face nil
                            :foreground keyword))
      (when (facep 'which-key-separator-face)
        (set-face-attribute 'which-key-separator-face nil
                            :foreground comment))
      
      ;; ========================================================================
      ;; EVIL (Vim keybindings)
      ;; ========================================================================
      
      (when (facep 'evil-ex-substitute-matches)
        (set-face-attribute 'evil-ex-substitute-matches nil
                            :background type :foreground bg))
      (when (facep 'evil-ex-substitute-replacement)
        (set-face-attribute 'evil-ex-substitute-replacement nil
                            :background string :foreground bg :weight 'bold))
      
      ;; ========================================================================
      ;; COMPANY (Autocomplete)
      ;; ========================================================================
      
      (when (facep 'company-tooltip)
        (set-face-attribute 'company-tooltip nil
                            :background bg-lighter :foreground fg))
      (when (facep 'company-tooltip-selection)
        (set-face-attribute 'company-tooltip-selection nil
                            :background selection :weight 'bold))
      (when (facep 'company-tooltip-common)
        (set-face-attribute 'company-tooltip-common nil
                            :foreground keyword :weight 'bold))
      (when (facep 'company-tooltip-common-selection)
        (set-face-attribute 'company-tooltip-common-selection nil
                            :foreground keyword-bright :weight 'bold))
      (when (facep 'company-tooltip-annotation)
        (set-face-attribute 'company-tooltip-annotation nil
                            :foreground comment))
      (when (facep 'company-scrollbar-bg)
        (set-face-attribute 'company-scrollbar-bg nil
                            :background bg-light))
      (when (facep 'company-scrollbar-fg)
        (set-face-attribute 'company-scrollbar-fg nil
                            :background keyword))
      
      ;; ========================================================================
      ;; FLYCHECK (Syntax checking)
      ;; ========================================================================
      
      (when (facep 'flycheck-error)
        (set-face-attribute 'flycheck-error nil
                            :underline (if (and type (not (equal type 'unspecified)) (stringp type))
                                           `(:style wave :color ,type)
                                         '(:style wave))))
      (when (facep 'flycheck-warning)
        (set-face-attribute 'flycheck-warning nil
                            :underline (if (and constant (not (equal constant 'unspecified)) (stringp constant))
                                           `(:style wave :color ,constant)
                                         '(:style wave))))
      (when (facep 'flycheck-info)
        (set-face-attribute 'flycheck-info nil
                            :underline (if (and keyword (not (equal keyword 'unspecified)) (stringp keyword))
                                           `(:style wave :color ,keyword)
                                         '(:style wave))))
      
      ;; ========================================================================
      ;; MARKDOWN
      ;; ========================================================================
      
      (when (facep 'markdown-header-face)
        (set-face-attribute 'markdown-header-face nil
                            :foreground keyword :weight 'bold))
      (when (facep 'markdown-header-delimiter-face)
        (set-face-attribute 'markdown-header-delimiter-face nil
                            :foreground comment))
      (when (facep 'markdown-blockquote-face)
        (set-face-attribute 'markdown-blockquote-face nil
                            :foreground fg-dim :slant 'italic))
      (when (facep 'markdown-list-face)
        (set-face-attribute 'markdown-list-face nil
                            :foreground keyword))
      (when (facep 'markdown-link-face)
        (set-face-attribute 'markdown-link-face nil
                            :foreground keyword :underline t))
      (when (facep 'markdown-url-face)
        (set-face-attribute 'markdown-url-face nil
                            :foreground builtin :underline t))
      (when (facep 'markdown-code-face)
        (set-face-attribute 'markdown-code-face nil
                            :foreground constant :background bg-light))
      
      ;; ========================================================================
      ;; MISC
      ;; ========================================================================
      
      ;; Highlight indent guides
      (when (facep 'highlight-indent-guides-character-face)
        (set-face-attribute 'highlight-indent-guides-character-face nil
                            :foreground bg-light))
      
      ;; Rainbow delimiters (subtle gradation)
      (when (facep 'rainbow-delimiters-depth-1-face)
        (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground keyword))
      (when (facep 'rainbow-delimiters-depth-2-face)
        (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground function))
      (when (facep 'rainbow-delimiters-depth-3-face)
        (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground string))
      (when (facep 'rainbow-delimiters-depth-4-face)
        (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground builtin))
      (when (facep 'rainbow-delimiters-depth-5-face)
        (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground constant))
      (when (facep 'rainbow-delimiters-depth-6-face)
        (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground type))
      (when (facep 'rainbow-delimiters-depth-7-face)
        (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground variable))
      
      ;; ========================================================================
      ;; CALFW (Calendar)
      ;; ========================================================================
      
      ;; Calendar faces - ensure they adapt to pywal colors dynamically
      (when (facep 'calfw-title-face)
        (set-face-attribute 'calfw-title-face nil
                            :foreground keyword :weight 'bold :height 1.5))
      (when (facep 'calfw-header-face)
        (set-face-attribute 'calfw-header-face nil
                            :foreground function :weight 'bold))
      (when (facep 'calfw-grid-face)
        (set-face-attribute 'calfw-grid-face nil
                            :foreground comment))
      (when (facep 'calfw-today-face)
        (set-face-attribute 'calfw-today-face nil
                            :background selection :foreground fg :weight 'bold))
      (when (facep 'calfw-today-title-face)
        (set-face-attribute 'calfw-today-title-face nil
                            :background selection :foreground fg :weight 'bold))
      (when (facep 'calfw-day-title-face)
        (set-face-attribute 'calfw-day-title-face nil
                            :foreground fg))
      (when (facep 'calfw-sunday-face)
        (set-face-attribute 'calfw-sunday-face nil
                            :foreground string :weight 'bold))
      (when (facep 'calfw-saturday-face)
        (set-face-attribute 'calfw-saturday-face nil
                            :foreground type :weight 'bold))
      (when (facep 'calfw-default-content-face)
        (set-face-attribute 'calfw-default-content-face nil
                            :foreground string))
      (when (facep 'calfw-periods-face)
        (set-face-attribute 'calfw-periods-face nil
                            :foreground keyword :slant 'italic))
      (when (facep 'calfw-holiday-face)
        (set-face-attribute 'calfw-holiday-face nil
                            :foreground string :weight 'bold))
      (when (facep 'calfw-toolbar-face)
        (set-face-attribute 'calfw-toolbar-face nil
                            :foreground fg :background bg-lighter))
      (when (facep 'calfw-toolbar-button-off-face)
        (set-face-attribute 'calfw-toolbar-button-off-face nil
                            :foreground comment :background bg-light))
      (when (facep 'calfw-toolbar-button-on-face)
        (set-face-attribute 'calfw-toolbar-button-on-face nil
                            :foreground fg :background bg-lighter :weight 'bold))
      
      (message "ewal-fix: Applied comprehensive face definitions"))))

;; Run after theme loads
(add-hook 'doom-load-theme-hook #'ewal-fix-comprehensive-faces)

(provide 'ewal-fix)
;;; ewal-fix.el ends here
