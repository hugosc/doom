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

(provide 'ewal-fix)
;;; ewal-fix.el ends here
