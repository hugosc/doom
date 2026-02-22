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
      
      ;; Org additional faces
      (when (facep 'org-headline-done)
        (set-face-attribute 'org-headline-done nil
                            :foreground comment))
      (when (facep 'org-checkbox-statistics-todo)
        (set-face-attribute 'org-checkbox-statistics-todo nil
                            :foreground string :weight 'bold))
      (when (facep 'org-checkbox-statistics-done)
        (set-face-attribute 'org-checkbox-statistics-done nil
                            :foreground comment :weight 'bold :strike-through t))
      (when (facep 'org-list-dt)
        (set-face-attribute 'org-list-dt nil
                            :foreground keyword :weight 'bold))
      (when (facep 'org-priority)
        (set-face-attribute 'org-priority nil
                            :foreground string :weight 'bold))
      (when (facep 'org-property-value)
        (set-face-attribute 'org-property-value nil
                            :foreground comment))
      (when (facep 'org-special-keyword)
        (set-face-attribute 'org-special-keyword nil
                            :foreground comment))
      (when (facep 'org-warning)
        (set-face-attribute 'org-warning nil
                            :foreground string :weight 'bold))
      (when (facep 'org-upcoming-deadline)
        (set-face-attribute 'org-upcoming-deadline nil
                            :foreground (doom-blend string fg 0.8)))
      (when (facep 'org-upcoming-distant-deadline)
        (set-face-attribute 'org-upcoming-distant-deadline nil
                            :foreground (doom-blend string fg 0.5)))
      (when (facep 'org-scheduled)
        (set-face-attribute 'org-scheduled nil
                            :foreground fg))
      (when (facep 'org-scheduled-today)
        (set-face-attribute 'org-scheduled-today nil
                            :foreground function))
      (when (facep 'org-scheduled-previously)
        (set-face-attribute 'org-scheduled-previously nil
                            :foreground (doom-darken string 0.2)))
      (when (facep 'org-time-grid)
        (set-face-attribute 'org-time-grid nil
                            :foreground comment))
      (when (facep 'org-cite)
        (set-face-attribute 'org-cite nil
                            :foreground (doom-blend type fg 0.9)))
      (when (facep 'org-cite-key)
        (set-face-attribute 'org-cite-key nil
                            :foreground (doom-blend type fg 0.6) :underline t))
      (when (facep 'org-footnote)
        (set-face-attribute 'org-footnote nil
                            :foreground builtin :underline t))
      (when (facep 'org-archived)
        (set-face-attribute 'org-archived nil
                            :foreground comment))

      ;; Org agenda
      (when (facep 'org-agenda-date)
        (set-face-attribute 'org-agenda-date nil :foreground keyword :weight 'ultra-bold))
      (when (facep 'org-agenda-date-today)
        (set-face-attribute 'org-agenda-date-today nil :foreground function :weight 'ultra-bold))
      (when (facep 'org-agenda-date-weekend)
        (set-face-attribute 'org-agenda-date-weekend nil :foreground (doom-darken keyword 0.2) :weight 'ultra-bold))
      (when (facep 'org-agenda-done)
        (set-face-attribute 'org-agenda-done nil :foreground comment))
      (when (facep 'org-agenda-dimmed-todo-face)
        (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground comment))
      (when (facep 'org-agenda-clocking)
        (set-face-attribute 'org-agenda-clocking nil :background (doom-blend builtin bg 0.2)))
      (when (facep 'org-agenda-structure)
        (set-face-attribute 'org-agenda-structure nil :foreground keyword :weight 'ultra-bold))
      
      ;; ========================================================================
      ;; MAGIT (Git interface)
      ;; ========================================================================
      
      (when (facep 'magit-section-heading)
        (set-face-attribute 'magit-section-heading nil
                            :foreground keyword :weight 'bold :extend t))
      (when (facep 'magit-section-heading-selection)
        (set-face-attribute 'magit-section-heading-selection nil
                            :foreground builtin :weight 'bold :extend t))
      (when (facep 'magit-section-secondary-heading)
        (set-face-attribute 'magit-section-secondary-heading nil
                            :foreground variable :weight 'bold :extend t))
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
      (when (facep 'magit-hash)
        (set-face-attribute 'magit-hash nil
                            :foreground comment))
      (when (facep 'magit-dimmed)
        (set-face-attribute 'magit-dimmed nil
                            :foreground comment))
      (when (facep 'magit-filename)
        (set-face-attribute 'magit-filename nil
                            :foreground variable))
      (when (facep 'magit-header-line)
        (set-face-attribute 'magit-header-line nil
                            :foreground fg :background bg-lighter :weight 'bold))

      ;; Magit blame
      (when (facep 'magit-blame-heading)
        (set-face-attribute 'magit-blame-heading nil
                            :foreground fg :background bg-lighter :extend t))
      (when (facep 'magit-blame-hash)
        (set-face-attribute 'magit-blame-hash nil
                            :foreground builtin))
      (when (facep 'magit-blame-date)
        (set-face-attribute 'magit-blame-date nil
                            :foreground type))

      ;; Magit bisect
      (when (facep 'magit-bisect-bad)
        (set-face-attribute 'magit-bisect-bad nil :foreground string))
      (when (facep 'magit-bisect-good)
        (set-face-attribute 'magit-bisect-good nil :foreground "#22aa22"))
      (when (facep 'magit-bisect-skip)
        (set-face-attribute 'magit-bisect-skip nil :foreground constant))

      ;; Magit log
      (when (facep 'magit-log-author)
        (set-face-attribute 'magit-log-author nil :foreground builtin))
      (when (facep 'magit-log-date)
        (set-face-attribute 'magit-log-date nil :foreground type))
      (when (facep 'magit-log-graph)
        (set-face-attribute 'magit-log-graph nil :foreground comment))

      ;; Magit process
      (when (facep 'magit-process-ok)
        (set-face-attribute 'magit-process-ok nil :foreground "#22aa22" :weight 'bold))
      (when (facep 'magit-process-ng)
        (set-face-attribute 'magit-process-ng nil :foreground string :weight 'bold))

      ;; Magit reflog
      (when (facep 'magit-reflog-commit)
        (set-face-attribute 'magit-reflog-commit nil :foreground "#22aa22"))
      (when (facep 'magit-reflog-amend)
        (set-face-attribute 'magit-reflog-amend nil :foreground variable))
      (when (facep 'magit-reflog-merge)
        (set-face-attribute 'magit-reflog-merge nil :foreground "#22aa22"))
      (when (facep 'magit-reflog-checkout)
        (set-face-attribute 'magit-reflog-checkout nil :foreground function))
      (when (facep 'magit-reflog-reset)
        (set-face-attribute 'magit-reflog-reset nil :foreground string))
      (when (facep 'magit-reflog-rebase)
        (set-face-attribute 'magit-reflog-rebase nil :foreground variable))
      (when (facep 'magit-reflog-cherry-pick)
        (set-face-attribute 'magit-reflog-cherry-pick nil :foreground "#22aa22"))
      (when (facep 'magit-reflog-remote)
        (set-face-attribute 'magit-reflog-remote nil :foreground builtin))
      (when (facep 'magit-reflog-other)
        (set-face-attribute 'magit-reflog-other nil :foreground type))
      (when (facep 'magit-refname)
        (set-face-attribute 'magit-refname nil :foreground comment))

      ;; Magit cherry
      (when (facep 'magit-cherry-equivalent)
        (set-face-attribute 'magit-cherry-equivalent nil :foreground variable))
      (when (facep 'magit-cherry-unmatched)
        (set-face-attribute 'magit-cherry-unmatched nil :foreground type))

      ;; Magit sequence
      (when (facep 'magit-sequence-head)
        (set-face-attribute 'magit-sequence-head nil :foreground function))
      (when (facep 'magit-sequence-part)
        (set-face-attribute 'magit-sequence-part nil :foreground constant))
      (when (facep 'magit-sequence-stop)
        (set-face-attribute 'magit-sequence-stop nil :foreground "#22aa22"))
      (when (facep 'magit-sequence-drop)
        (set-face-attribute 'magit-sequence-drop nil :foreground string))

      ;; Magit signatures
      (when (facep 'magit-signature-good)
        (set-face-attribute 'magit-signature-good nil :foreground "#22aa22"))
      (when (facep 'magit-signature-bad)
        (set-face-attribute 'magit-signature-bad nil :foreground string :weight 'bold))
      (when (facep 'magit-signature-error)
        (set-face-attribute 'magit-signature-error nil :foreground string))
      (when (facep 'magit-signature-expired)
        (set-face-attribute 'magit-signature-expired nil :foreground constant))
      (when (facep 'magit-signature-revoked)
        (set-face-attribute 'magit-signature-revoked nil :foreground variable))
      (when (facep 'magit-signature-untrusted)
        (set-face-attribute 'magit-signature-untrusted nil :foreground type))

      ;; Magit diff faces - always green for added, red for removed (not theme-dependent)
      (when (facep 'magit-diff-added)
        (set-face-attribute 'magit-diff-added nil
                            :foreground (doom-darken "#22aa22" 0.2)
                            :background (doom-blend "#22aa22" bg 0.1) :extend t))
      (when (facep 'magit-diff-added-highlight)
        (set-face-attribute 'magit-diff-added-highlight nil
                            :foreground "#22aa22"
                            :background (doom-blend "#22aa22" bg 0.2) :weight 'bold :extend t))
      (when (facep 'magit-diff-removed)
        (set-face-attribute 'magit-diff-removed nil
                            :foreground (doom-darken "#aa2222" 0.2)
                            :background (doom-blend "#aa2222" bg 0.1) :extend t))
      (when (facep 'magit-diff-removed-highlight)
        (set-face-attribute 'magit-diff-removed-highlight nil
                            :foreground "#aa2222"
                            :background (doom-blend "#aa2222" bg 0.2) :weight 'bold :extend t))

      ;; Magit diff context / hunk
      (when (facep 'magit-diff-context)
        (set-face-attribute 'magit-diff-context nil
                            :foreground (doom-blend fg bg 0.6) :background bg :extend t))
      (when (facep 'magit-diff-context-highlight)
        (set-face-attribute 'magit-diff-context-highlight nil
                            :foreground fg :background bg-light :extend t))
      (when (facep 'magit-diff-hunk-heading)
        (set-face-attribute 'magit-diff-hunk-heading nil
                            :foreground bg :background (doom-blend variable bg 0.3) :extend t))
      (when (facep 'magit-diff-hunk-heading-highlight)
        (set-face-attribute 'magit-diff-hunk-heading-highlight nil
                            :foreground bg :background variable :weight 'bold :extend t))
      (when (facep 'magit-diff-file-heading)
        (set-face-attribute 'magit-diff-file-heading nil
                            :foreground fg :weight 'bold :extend t))
      (when (facep 'magit-diff-file-heading-selection)
        (set-face-attribute 'magit-diff-file-heading-selection nil
                            :foreground builtin :weight 'bold :extend t))
      (when (facep 'magit-diff-base)
        (set-face-attribute 'magit-diff-base nil
                            :foreground (doom-darken constant 0.2)
                            :background (doom-blend constant bg 0.1) :extend t))
      (when (facep 'magit-diff-base-highlight)
        (set-face-attribute 'magit-diff-base-highlight nil
                            :foreground constant
                            :background (doom-blend constant bg 0.2) :weight 'bold :extend t))
      (when (facep 'magit-diff-lines-heading)
        (set-face-attribute 'magit-diff-lines-heading nil
                            :foreground constant :background string :extend t))

      ;; Magit diffstat
      (when (facep 'magit-diffstat-added)
        (set-face-attribute 'magit-diffstat-added nil :foreground "#22aa22"))
      (when (facep 'magit-diffstat-removed)
        (set-face-attribute 'magit-diffstat-removed nil :foreground "#aa2222"))
      
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
      (when (facep 'rainbow-delimiters-depth-8-face)
        (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground keyword))
      (when (facep 'rainbow-delimiters-depth-9-face)
        (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground function))
      (when (facep 'rainbow-delimiters-unmatched-face)
        (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                            :foreground string :weight 'bold :inverse-video t))
      (when (facep 'rainbow-delimiters-mismatched-face)
        (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                            :foreground string :weight 'bold :inverse-video t))
      (when (facep 'rainbow-delimiters-base-error-face)
        (set-face-attribute 'rainbow-delimiters-base-error-face nil
                            :foreground string))
      
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
      
      ;; ========================================================================
      ;; OUTLINE (org-level-N inherit these)
      ;; ========================================================================

      (when (facep 'outline-1)
        (set-face-attribute 'outline-1 nil :foreground keyword :weight 'bold :extend t))
      (when (facep 'outline-2)
        (set-face-attribute 'outline-2 nil :foreground function :weight 'bold :extend t))
      (when (facep 'outline-3)
        (set-face-attribute 'outline-3 nil :foreground builtin :weight 'bold :extend t))
      (when (facep 'outline-4)
        (set-face-attribute 'outline-4 nil :foreground variable :weight 'bold :extend t))
      (when (facep 'outline-5)
        (set-face-attribute 'outline-5 nil :foreground type :weight 'bold :extend t))
      (when (facep 'outline-6)
        (set-face-attribute 'outline-6 nil :foreground string :weight 'bold :extend t))
      (when (facep 'outline-7)
        (set-face-attribute 'outline-7 nil :foreground constant :weight 'bold :extend t))
      (when (facep 'outline-8)
        (set-face-attribute 'outline-8 nil :foreground comment :weight 'bold :extend t))

      ;; ========================================================================
      ;; VTERM
      ;; ========================================================================

      (when (facep 'vterm-color-black)
        (set-face-attribute 'vterm-color-black nil
                            :foreground bg :background (doom-lighten bg 0.25)))
      (when (facep 'vterm-color-red)
        (set-face-attribute 'vterm-color-red nil
                            :foreground string :background (doom-lighten string 0.25)))
      (when (facep 'vterm-color-green)
        (set-face-attribute 'vterm-color-green nil
                            :foreground "#22aa22" :background (doom-lighten "#22aa22" 0.25)))
      (when (facep 'vterm-color-yellow)
        (set-face-attribute 'vterm-color-yellow nil
                            :foreground constant :background (doom-lighten constant 0.25)))
      (when (facep 'vterm-color-blue)
        (set-face-attribute 'vterm-color-blue nil
                            :foreground function :background (doom-lighten function 0.25)))
      (when (facep 'vterm-color-magenta)
        (set-face-attribute 'vterm-color-magenta nil
                            :foreground variable :background (doom-lighten variable 0.25)))
      (when (facep 'vterm-color-cyan)
        (set-face-attribute 'vterm-color-cyan nil
                            :foreground type :background (doom-lighten type 0.25)))
      (when (facep 'vterm-color-white)
        (set-face-attribute 'vterm-color-white nil
                            :foreground fg :background (doom-lighten fg 0.25)))

      ;; ========================================================================
      ;; TOOLTIP / FRINGE
      ;; ========================================================================

      (set-face-attribute 'tooltip nil
                          :background bg-lighter :foreground fg)
      (set-face-attribute 'fringe nil
                          :foreground comment :background bg)

      ;; ========================================================================
      ;; ISEARCH
      ;; ========================================================================

      (when (facep 'isearch-fail)
        (set-face-attribute 'isearch-fail nil
                            :background string :foreground bg :weight 'bold))

      ;; ========================================================================
      ;; FLYCHECK fringe indicators
      ;; ========================================================================

      (when (facep 'flycheck-fringe-error)
        (set-face-attribute 'flycheck-fringe-error nil :foreground string))
      (when (facep 'flycheck-fringe-warning)
        (set-face-attribute 'flycheck-fringe-warning nil :foreground constant))
      (when (facep 'flycheck-fringe-info)
        (set-face-attribute 'flycheck-fringe-info nil :foreground function))

      ;; ========================================================================
      ;; HL-TODO
      ;; ========================================================================

      (when (facep 'hl-todo)
        (set-face-attribute 'hl-todo nil :foreground string :weight 'bold))

      ;; ========================================================================
      ;; DIFF-HL / GIT-GUTTER
      ;; ========================================================================

      (when (facep 'diff-hl-change)
        (set-face-attribute 'diff-hl-change nil :foreground constant :background constant))
      (when (facep 'diff-hl-insert)
        (set-face-attribute 'diff-hl-insert nil :foreground "#22aa22" :background "#22aa22"))
      (when (facep 'diff-hl-delete)
        (set-face-attribute 'diff-hl-delete nil :foreground "#aa2222" :background "#aa2222"))
      (when (facep 'git-gutter:modified)
        (set-face-attribute 'git-gutter:modified nil :foreground constant))
      (when (facep 'git-gutter:added)
        (set-face-attribute 'git-gutter:added nil :foreground "#22aa22"))
      (when (facep 'git-gutter:deleted)
        (set-face-attribute 'git-gutter:deleted nil :foreground "#aa2222"))

      ;; ========================================================================
      ;; VERTICO / ORDERLESS
      ;; ========================================================================

      (when (facep 'vertico-current)
        (set-face-attribute 'vertico-current nil :background selection :extend t))
      (when (facep 'orderless-match-face-0)
        (set-face-attribute 'orderless-match-face-0 nil
                            :weight 'bold
                            :foreground (doom-blend function fg 0.6)
                            :background (doom-blend function bg 0.1)))
      (when (facep 'orderless-match-face-1)
        (set-face-attribute 'orderless-match-face-1 nil
                            :weight 'bold
                            :foreground (doom-blend variable fg 0.6)
                            :background (doom-blend variable bg 0.1)))
      (when (facep 'orderless-match-face-2)
        (set-face-attribute 'orderless-match-face-2 nil
                            :weight 'bold
                            :foreground (doom-blend builtin fg 0.6)
                            :background (doom-blend builtin bg 0.1)))
      (when (facep 'orderless-match-face-3)
        (set-face-attribute 'orderless-match-face-3 nil
                            :weight 'bold
                            :foreground (doom-blend constant fg 0.6)
                            :background (doom-blend constant bg 0.1)))

      ;; ========================================================================
      ;; DOOM DASHBOARD
      ;; ========================================================================

      (when (facep 'doom-dashboard-menu-title)
        (set-face-attribute 'doom-dashboard-menu-title nil :foreground function))
      (when (facep 'doom-dashboard-menu-desc)
        (set-face-attribute 'doom-dashboard-menu-desc nil :foreground keyword))
      (when (facep 'doom-dashboard-footer-icon)
        (set-face-attribute 'doom-dashboard-footer-icon nil :foreground comment))
      (when (facep 'doom-dashboard-loaded)
        (set-face-attribute 'doom-dashboard-loaded nil :foreground constant))
      (when (facep 'doom-dashboard-banner)
        (set-face-attribute 'doom-dashboard-banner nil :foreground (doom-blend comment bg 0.5)))

      ;; ========================================================================
      ;; MARKDOWN per-level headers
      ;; ========================================================================

      (when (facep 'markdown-header-face-1)
        (set-face-attribute 'markdown-header-face-1 nil :foreground keyword :weight 'bold))
      (when (facep 'markdown-header-face-2)
        (set-face-attribute 'markdown-header-face-2 nil :foreground function :weight 'bold))
      (when (facep 'markdown-header-face-3)
        (set-face-attribute 'markdown-header-face-3 nil :foreground builtin :weight 'bold))
      (when (facep 'markdown-header-face-4)
        (set-face-attribute 'markdown-header-face-4 nil :foreground variable :weight 'bold))
      (when (facep 'markdown-header-face-5)
        (set-face-attribute 'markdown-header-face-5 nil :foreground type))
      (when (facep 'markdown-header-face-6)
        (set-face-attribute 'markdown-header-face-6 nil :foreground string))
      (when (facep 'markdown-markup-face)
        (set-face-attribute 'markdown-markup-face nil :foreground comment))
      (when (facep 'markdown-inline-code-face)
        (set-face-attribute 'markdown-inline-code-face nil
                            :foreground constant :background bg-light))
      (when (facep 'markdown-italic-face)
        (set-face-attribute 'markdown-italic-face nil :foreground variable :slant 'italic))
      (when (facep 'markdown-bold-face)
        (set-face-attribute 'markdown-bold-face nil :foreground builtin :weight 'bold))
      (when (facep 'markdown-metadata-key-face)
        (set-face-attribute 'markdown-metadata-key-face nil :foreground string))

      ;; ========================================================================
      ;; ANSI-COLOR / TERM (built-in terminal emulator faces)
      ;; ========================================================================

      (set-face-attribute 'ansi-color-black nil
                          :foreground bg :background bg)
      (set-face-attribute 'ansi-color-red nil
                          :foreground string :background string)
      (set-face-attribute 'ansi-color-green nil
                          :foreground "#22aa22" :background "#22aa22")
      (set-face-attribute 'ansi-color-yellow nil
                          :foreground constant :background constant)
      (set-face-attribute 'ansi-color-blue nil
                          :foreground function :background function)
      (set-face-attribute 'ansi-color-magenta nil
                          :foreground variable :background variable)
      (set-face-attribute 'ansi-color-cyan nil
                          :foreground type :background type)
      (set-face-attribute 'ansi-color-white nil
                          :foreground fg :background fg)
      (set-face-attribute 'ansi-color-bright-black nil
                          :foreground comment :background comment)
      (set-face-attribute 'ansi-color-bright-red nil
                          :foreground (doom-lighten string 0.15)
                          :background (doom-lighten string 0.15))
      (set-face-attribute 'ansi-color-bright-green nil
                          :foreground (doom-lighten "#22aa22" 0.15)
                          :background (doom-lighten "#22aa22" 0.15))
      (set-face-attribute 'ansi-color-bright-yellow nil
                          :foreground (doom-lighten constant 0.15)
                          :background (doom-lighten constant 0.15))
      (set-face-attribute 'ansi-color-bright-blue nil
                          :foreground (doom-lighten function 0.15)
                          :background (doom-lighten function 0.15))
      (set-face-attribute 'ansi-color-bright-magenta nil
                          :foreground (doom-lighten variable 0.15)
                          :background (doom-lighten variable 0.15))
      (set-face-attribute 'ansi-color-bright-cyan nil
                          :foreground (doom-lighten type 0.15)
                          :background (doom-lighten type 0.15))
      (set-face-attribute 'ansi-color-bright-white nil
                          :foreground fg-dim :background fg-dim)

      ;; term faces mirror ansi-color
      (when (facep 'term-color-black)
        (set-face-attribute 'term-color-black nil
                            :foreground bg :background bg))
      (when (facep 'term-color-red)
        (set-face-attribute 'term-color-red nil
                            :foreground string :background string))
      (when (facep 'term-color-green)
        (set-face-attribute 'term-color-green nil
                            :foreground "#22aa22" :background "#22aa22"))
      (when (facep 'term-color-yellow)
        (set-face-attribute 'term-color-yellow nil
                            :foreground constant :background constant))
      (when (facep 'term-color-blue)
        (set-face-attribute 'term-color-blue nil
                            :foreground function :background function))
      (when (facep 'term-color-magenta)
        (set-face-attribute 'term-color-magenta nil
                            :foreground variable :background variable))
      (when (facep 'term-color-cyan)
        (set-face-attribute 'term-color-cyan nil
                            :foreground type :background type))
      (when (facep 'term-color-white)
        (set-face-attribute 'term-color-white nil
                            :foreground fg :background fg))

      ;; ========================================================================
      ;; ESHELL (built-in shell)
      ;; ========================================================================

      (when (facep 'eshell-prompt)
        (set-face-attribute 'eshell-prompt nil
                            :foreground keyword :weight 'bold))
      (when (facep 'eshell-ls-directory)
        (set-face-attribute 'eshell-ls-directory nil
                            :foreground function :weight 'bold))
      (when (facep 'eshell-ls-symlink)
        (set-face-attribute 'eshell-ls-symlink nil
                            :foreground builtin :weight 'bold))
      (when (facep 'eshell-ls-executable)
        (set-face-attribute 'eshell-ls-executable nil
                            :foreground "#22aa22" :weight 'bold))
      (when (facep 'eshell-ls-missing)
        (set-face-attribute 'eshell-ls-missing nil
                            :foreground string))
      (when (facep 'eshell-ls-archive)
        (set-face-attribute 'eshell-ls-archive nil
                            :foreground variable))
      (when (facep 'eshell-ls-backup)
        (set-face-attribute 'eshell-ls-backup nil
                            :foreground comment))
      (when (facep 'eshell-ls-clutter)
        (set-face-attribute 'eshell-ls-clutter nil
                            :foreground string))
      (when (facep 'eshell-ls-product)
        (set-face-attribute 'eshell-ls-product nil
                            :foreground constant))
      (when (facep 'eshell-ls-readonly)
        (set-face-attribute 'eshell-ls-readonly nil
                            :foreground constant))
      (when (facep 'eshell-ls-special)
        (set-face-attribute 'eshell-ls-special nil
                            :foreground variable))
      (when (facep 'eshell-ls-unreadable)
        (set-face-attribute 'eshell-ls-unreadable nil
                            :foreground comment))

      ;; ========================================================================
      ;; COMPILATION (built-in)
      ;; ========================================================================

      (when (facep 'compilation-error)
        (set-face-attribute 'compilation-error nil
                            :foreground string :weight 'bold))
      (when (facep 'compilation-warning)
        (set-face-attribute 'compilation-warning nil
                            :foreground constant :slant 'italic))
      (when (facep 'compilation-info)
        (set-face-attribute 'compilation-info nil
                            :foreground "#22aa22"))
      (when (facep 'compilation-line-number)
        (set-face-attribute 'compilation-line-number nil
                            :foreground keyword))
      (when (facep 'compilation-column-number)
        (set-face-attribute 'compilation-column-number nil
                            :foreground comment))
      (when (facep 'compilation-mode-line-exit)
        (set-face-attribute 'compilation-mode-line-exit nil
                            :foreground "#22aa22" :weight 'bold))
      (when (facep 'compilation-mode-line-fail)
        (set-face-attribute 'compilation-mode-line-fail nil
                            :foreground string :weight 'bold))

      ;; ========================================================================
      ;; ANZU (incremental search count)
      ;; ========================================================================

      (when (facep 'anzu-replace-highlight)
        (set-face-attribute 'anzu-replace-highlight nil
                            :background bg-light :foreground string
                            :weight 'bold :strike-through t))
      (when (facep 'anzu-replace-to)
        (set-face-attribute 'anzu-replace-to nil
                            :background bg-light :foreground "#22aa22"
                            :weight 'bold))

      ;; ========================================================================
      ;; AVY (jump-to positions)
      ;; ========================================================================

      (when (facep 'avy-background-face)
        (set-face-attribute 'avy-background-face nil
                            :foreground comment))
      (when (facep 'avy-lead-face)
        (set-face-attribute 'avy-lead-face nil
                            :background keyword :foreground bg :weight 'bold))
      (when (facep 'avy-lead-face-0)
        (set-face-attribute 'avy-lead-face-0 nil
                            :background (doom-lighten keyword 0.3) :foreground bg :weight 'bold))
      (when (facep 'avy-lead-face-1)
        (set-face-attribute 'avy-lead-face-1 nil
                            :background (doom-lighten keyword 0.6) :foreground bg :weight 'bold))
      (when (facep 'avy-lead-face-2)
        (set-face-attribute 'avy-lead-face-2 nil
                            :background (doom-lighten keyword 0.9) :foreground bg :weight 'bold))

      ;; ========================================================================
      ;; CORFU (completion popup)
      ;; ========================================================================

      (when (facep 'corfu-default)
        (set-face-attribute 'corfu-default nil
                            :background bg-lighter :foreground fg))
      (when (facep 'corfu-current)
        (set-face-attribute 'corfu-current nil
                            :background selection :foreground fg :weight 'bold))
      (when (facep 'corfu-bar)
        (set-face-attribute 'corfu-bar nil
                            :background keyword))
      (when (facep 'corfu-border)
        (set-face-attribute 'corfu-border nil
                            :background bg-lighter))
      (when (facep 'corfu-annotations)
        (set-face-attribute 'corfu-annotations nil
                            :foreground comment))
      (when (facep 'corfu-deprecated)
        (set-face-attribute 'corfu-deprecated nil
                            :foreground comment :strike-through t))

      ;; ========================================================================
      ;; LSP-MODE / LSP-UI
      ;; ========================================================================

      (when (facep 'lsp-face-highlight-textual)
        (set-face-attribute 'lsp-face-highlight-textual nil
                            :background (doom-blend keyword bg 0.3) :weight 'bold))
      (when (facep 'lsp-face-highlight-read)
        (set-face-attribute 'lsp-face-highlight-read nil
                            :background (doom-blend keyword bg 0.3) :weight 'bold))
      (when (facep 'lsp-face-highlight-write)
        (set-face-attribute 'lsp-face-highlight-write nil
                            :background (doom-blend keyword bg 0.3) :weight 'bold))
      (when (facep 'lsp-ui-doc-background)
        (set-face-attribute 'lsp-ui-doc-background nil
                            :background bg-lighter :foreground fg))
      (when (facep 'lsp-ui-peek-header)
        (set-face-attribute 'lsp-ui-peek-header nil
                            :foreground fg :background bg-lighter :weight 'bold))
      (when (facep 'lsp-ui-peek-selection)
        (set-face-attribute 'lsp-ui-peek-selection nil
                            :background function :foreground bg :weight 'bold))
      (when (facep 'lsp-ui-peek-list)
        (set-face-attribute 'lsp-ui-peek-list nil
                            :background (doom-darken bg 0.1)))
      (when (facep 'lsp-ui-peek-peek)
        (set-face-attribute 'lsp-ui-peek-peek nil
                            :background (doom-darken bg 0.1)))
      (when (facep 'lsp-ui-peek-highlight)
        (set-face-attribute 'lsp-ui-peek-highlight nil
                            :background keyword :foreground bg))
      (when (facep 'lsp-ui-peek-filename)
        (set-face-attribute 'lsp-ui-peek-filename nil
                            :foreground function :weight 'bold))
      (when (facep 'lsp-ui-sideline-code-action)
        (set-face-attribute 'lsp-ui-sideline-code-action nil
                            :foreground (doom-blend keyword bg 0.85)))
      (when (facep 'lsp-ui-sideline-current-symbol)
        (set-face-attribute 'lsp-ui-sideline-current-symbol nil
                            :background selection))
      (when (facep 'lsp-ui-sideline-symbol-info)
        (set-face-attribute 'lsp-ui-sideline-symbol-info nil
                            :foreground (doom-blend comment bg 0.85)
                            :background bg-light :extend t))
      (when (facep 'lsp-headerline-breadcrumb-separator-face)
        (set-face-attribute 'lsp-headerline-breadcrumb-separator-face nil
                            :foreground comment))

      ;; ========================================================================
      ;; SMERGE (merge conflict resolution)
      ;; ========================================================================

      (when (facep 'smerge-lower)
        (set-face-attribute 'smerge-lower nil
                            :background (doom-blend "#22aa22" bg 0.2)))
      (when (facep 'smerge-upper)
        (set-face-attribute 'smerge-upper nil
                            :background (doom-blend "#aa2222" bg-light 0.2)))
      (when (facep 'smerge-base)
        (set-face-attribute 'smerge-base nil
                            :background (doom-blend function bg 0.2)))
      (when (facep 'smerge-markers)
        (set-face-attribute 'smerge-markers nil
                            :background comment :foreground bg :weight 'bold))
      (when (facep 'smerge-refined-added)
        (set-face-attribute 'smerge-refined-added nil
                            :foreground "#22aa22" :inverse-video t))
      (when (facep 'smerge-refined-removed)
        (set-face-attribute 'smerge-refined-removed nil
                            :foreground "#aa2222" :inverse-video t))

      ;; ========================================================================
      ;; WGREP (writable grep results)
      ;; ========================================================================

      (when (facep 'wgrep-face)
        (set-face-attribute 'wgrep-face nil
                            :weight 'bold :foreground "#22aa22" :background bg-lighter))
      (when (facep 'wgrep-delete-face)
        (set-face-attribute 'wgrep-delete-face nil
                            :foreground bg-light :background "#aa2222"))
      (when (facep 'wgrep-done-face)
        (set-face-attribute 'wgrep-done-face nil
                            :foreground function))
      (when (facep 'wgrep-file-face)
        (set-face-attribute 'wgrep-file-face nil
                            :foreground comment))
      (when (facep 'wgrep-reject-face)
        (set-face-attribute 'wgrep-reject-face nil
                            :foreground string :weight 'bold))

      ;; ========================================================================
      ;; ELFEED (RSS/Atom feed reader)
      ;; ========================================================================

      (when (facep 'elfeed-search-date-face)
        (set-face-attribute 'elfeed-search-date-face nil
                            :foreground type))
      (when (facep 'elfeed-search-feed-face)
        (set-face-attribute 'elfeed-search-feed-face nil
                            :foreground function))
      (when (facep 'elfeed-search-tag-face)
        (set-face-attribute 'elfeed-search-tag-face nil
                            :foreground comment))
      (when (facep 'elfeed-search-title-face)
        (set-face-attribute 'elfeed-search-title-face nil
                            :foreground comment))
      (when (facep 'elfeed-search-unread-title-face)
        (set-face-attribute 'elfeed-search-unread-title-face nil
                            :foreground fg :weight 'bold))
      (when (facep 'elfeed-search-unread-count-face)
        (set-face-attribute 'elfeed-search-unread-count-face nil
                            :foreground constant))
      (when (facep 'elfeed-search-filter-face)
        (set-face-attribute 'elfeed-search-filter-face nil
                            :foreground variable))
      (when (facep 'elfeed-log-error-level-face)
        (set-face-attribute 'elfeed-log-error-level-face nil
                            :foreground string))
      (when (facep 'elfeed-log-warn-level-face)
        (set-face-attribute 'elfeed-log-warn-level-face nil
                            :foreground constant))
      (when (facep 'elfeed-log-info-level-face)
        (set-face-attribute 'elfeed-log-info-level-face nil
                            :foreground "#22aa22"))
      (when (facep 'elfeed-log-debug-level-face)
        (set-face-attribute 'elfeed-log-debug-level-face nil
                            :foreground comment))

      ;; ========================================================================
      ;; GIT-COMMIT (commit message editing)
      ;; ========================================================================

      (when (facep 'git-commit-summary)
        (set-face-attribute 'git-commit-summary nil
                            :foreground string))
      (when (facep 'git-commit-overlong-summary)
        (set-face-attribute 'git-commit-overlong-summary nil
                            :foreground string :background bg-light :slant 'italic :weight 'bold))
      (when (facep 'git-commit-nonempty-second-line)
        (set-face-attribute 'git-commit-nonempty-second-line nil
                            :foreground string :background bg-light :slant 'italic :weight 'bold))
      (when (facep 'git-commit-keyword)
        (set-face-attribute 'git-commit-keyword nil
                            :foreground type :slant 'italic))
      (when (facep 'git-commit-pseudo-header)
        (set-face-attribute 'git-commit-pseudo-header nil
                            :foreground comment :slant 'italic))
      (when (facep 'git-commit-known-pseudo-header)
        (set-face-attribute 'git-commit-known-pseudo-header nil
                            :foreground comment :weight 'bold :slant 'italic))
      (when (facep 'git-commit-comment-branch-local)
        (set-face-attribute 'git-commit-comment-branch-local nil
                            :foreground function))
      (when (facep 'git-commit-comment-branch-remote)
        (set-face-attribute 'git-commit-comment-branch-remote nil
                            :foreground builtin))
      (when (facep 'git-commit-comment-detached)
        (set-face-attribute 'git-commit-comment-detached nil
                            :foreground constant))
      (when (facep 'git-commit-comment-heading)
        (set-face-attribute 'git-commit-comment-heading nil
                            :foreground keyword))
      (when (facep 'git-commit-comment-file)
        (set-face-attribute 'git-commit-comment-file nil
                            :foreground variable))

      ;; ========================================================================
      ;; TAB-LINE / TAB-BAR (Emacs 27+ built-in tabs)
      ;; ========================================================================

      (when (facep 'tab-line)
        (set-face-attribute 'tab-line nil
                            :background bg-light :foreground comment))
      (when (facep 'tab-line-tab)
        (set-face-attribute 'tab-line-tab nil
                            :background bg :foreground fg))
      (when (facep 'tab-line-tab-current)
        (set-face-attribute 'tab-line-tab-current nil
                            :background bg :foreground fg))
      (when (facep 'tab-line-tab-inactive)
        (set-face-attribute 'tab-line-tab-inactive nil
                            :background bg-light :foreground comment))
      (when (facep 'tab-line-highlight)
        (set-face-attribute 'tab-line-highlight nil
                            :background selection :foreground fg))
      (when (facep 'tab-line-close-highlight)
        (set-face-attribute 'tab-line-close-highlight nil
                            :foreground string))
      (when (facep 'tab-bar)
        (set-face-attribute 'tab-bar nil
                            :background bg-light :foreground comment))
      (when (facep 'tab-bar-tab)
        (set-face-attribute 'tab-bar-tab nil
                            :background bg :foreground fg :weight 'bold))
      (when (facep 'tab-bar-tab-inactive)
        (set-face-attribute 'tab-bar-tab-inactive nil
                            :background bg-light :foreground comment))

      ;; ========================================================================
      ;; ORG-HABIT
      ;; ========================================================================

      (when (facep 'org-habit-clear-face)
        (set-face-attribute 'org-habit-clear-face nil
                            :weight 'bold :background bg-lighter))
      (when (facep 'org-habit-clear-future-face)
        (set-face-attribute 'org-habit-clear-future-face nil
                            :weight 'bold :background bg-light))
      (when (facep 'org-habit-ready-face)
        (set-face-attribute 'org-habit-ready-face nil
                            :weight 'bold :background (doom-blend function bg 0.5)))
      (when (facep 'org-habit-ready-future-face)
        (set-face-attribute 'org-habit-ready-future-face nil
                            :weight 'bold :background (doom-blend function bg 0.3)))
      (when (facep 'org-habit-alert-face)
        (set-face-attribute 'org-habit-alert-face nil
                            :weight 'bold :background (doom-blend constant bg 0.5)))
      (when (facep 'org-habit-alert-future-face)
        (set-face-attribute 'org-habit-alert-future-face nil
                            :weight 'bold :background (doom-blend constant bg 0.3)))
      (when (facep 'org-habit-overdue-face)
        (set-face-attribute 'org-habit-overdue-face nil
                            :weight 'bold :background (doom-blend string bg 0.5)))
      (when (facep 'org-habit-overdue-future-face)
        (set-face-attribute 'org-habit-overdue-future-face nil
                            :weight 'bold :background (doom-blend string bg 0.3)))

      ;; ========================================================================
      ;; MARGINALIA (minibuffer annotations)
      ;; ========================================================================

      (when (facep 'marginalia-documentation)
        (set-face-attribute 'marginalia-documentation nil
                            :foreground comment :slant 'italic))
      (when (facep 'marginalia-file-priv-dir)
        (set-face-attribute 'marginalia-file-priv-dir nil
                            :foreground function))
      (when (facep 'marginalia-file-priv-exec)
        (set-face-attribute 'marginalia-file-priv-exec nil
                            :foreground "#22aa22"))
      (when (facep 'marginalia-file-priv-link)
        (set-face-attribute 'marginalia-file-priv-link nil
                            :foreground builtin))
      (when (facep 'marginalia-file-priv-other)
        (set-face-attribute 'marginalia-file-priv-other nil
                            :foreground variable))
      (when (facep 'marginalia-file-priv-rare)
        (set-face-attribute 'marginalia-file-priv-rare nil
                            :foreground fg))
      (when (facep 'marginalia-file-priv-read)
        (set-face-attribute 'marginalia-file-priv-read nil
                            :foreground constant))
      (when (facep 'marginalia-file-priv-write)
        (set-face-attribute 'marginalia-file-priv-write nil
                            :foreground string))
      (when (facep 'marginalia-number)
        (set-face-attribute 'marginalia-number nil
                            :foreground constant))
      (when (facep 'marginalia-size)
        (set-face-attribute 'marginalia-size nil
                            :foreground variable))
      (when (facep 'marginalia-lighter)
        (set-face-attribute 'marginalia-lighter nil
                            :foreground comment))

      ;; ========================================================================
      ;; SYMBOL-OVERLAY (highlight occurrences of symbol at point)
      ;; ========================================================================

      (when (facep 'symbol-overlay-default-face)
        (set-face-attribute 'symbol-overlay-default-face nil
                            :background (doom-lighten selection 0.1)))
      (when (facep 'symbol-overlay-face-1)
        (set-face-attribute 'symbol-overlay-face-1 nil
                            :background (doom-blend function bg 0.4)))
      (when (facep 'symbol-overlay-face-2)
        (set-face-attribute 'symbol-overlay-face-2 nil
                            :background (doom-blend variable bg 0.4)))
      (when (facep 'symbol-overlay-face-3)
        (set-face-attribute 'symbol-overlay-face-3 nil
                            :background (doom-blend constant bg 0.3)))
      (when (facep 'symbol-overlay-face-4)
        (set-face-attribute 'symbol-overlay-face-4 nil
                            :background (doom-blend type bg 0.3)))
      (when (facep 'symbol-overlay-face-5)
        (set-face-attribute 'symbol-overlay-face-5 nil
                            :background (doom-blend string bg 0.3)))
      (when (facep 'symbol-overlay-face-6)
        (set-face-attribute 'symbol-overlay-face-6 nil
                            :background (doom-blend builtin bg 0.3)))
      (when (facep 'symbol-overlay-face-7)
        (set-face-attribute 'symbol-overlay-face-7 nil
                            :background (doom-blend "#22aa22" bg 0.4)))
      (when (facep 'symbol-overlay-face-8)
        (set-face-attribute 'symbol-overlay-face-8 nil
                            :background (doom-blend keyword bg 0.2)))

      (message "ewal-fix: Applied comprehensive face definitions"))))

;; Run after theme loads
(add-hook 'doom-load-theme-hook #'ewal-fix-comprehensive-faces)

(provide 'ewal-fix)
;;; ewal-fix.el ends here
