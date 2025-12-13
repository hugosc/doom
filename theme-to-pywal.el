;;; theme-to-pywal.el --- Extract theme colors and generate pywal JSON -*- lexical-binding: t; -*-

;;; Commentary:
;; This script extracts colors from Emacs theme files and generates pywal JSON presets.
;; Supports: doom-themes, built-in Emacs themes, and other third-party themes.

;;; Code:

(require 'json)
(require 'cl-lib)

(defun theme-to-pywal--color-to-hex (color)
  "Convert COLOR (hex or named) to hex format.
Returns COLOR unchanged if already hex, converts named colors to hex."
  (if (and (stringp color)
           (string-match "^#[0-9a-fA-F]\\{6\\}$" color))
      ;; Already hex
      color
    ;; Try to convert named color to hex
    (condition-case nil
        (let* ((rgb (color-name-to-rgb color))
               (r (round (* 255 (nth 0 rgb))))
               (g (round (* 255 (nth 1 rgb))))
               (b (round (* 255 (nth 2 rgb)))))
          (format "#%02x%02x%02x" r g b))
      (error color)))) ;; Return original if conversion fails

(defun theme-to-pywal--extract-doom-theme-colors (theme-file)
  "Extract colors from a doom-theme THEME-FILE.
Returns an alist of (color-name . hex-value)."
  (with-temp-buffer
    (insert-file-contents theme-file)
    (goto-char (point-min))
    (let ((colors '()))
      ;; Extract color definitions from doom theme
      ;; Format: (color-name '("#hex" "256-color" "16-color"))
      (while (re-search-forward
              "^[[:space:]]*(\\([a-z-]+\\)[[:space:]]+[^']*'(\"\\(#[0-9a-fA-F]\\{6\\}\\)\"" nil t)
        (let ((name (match-string 1))
              (hex (downcase (match-string 2))))
          (push (cons name hex) colors)))
      (nreverse colors))))

(defun theme-to-pywal--extract-standard-theme-colors (theme-name)
  "Extract colors from a loaded standard Emacs THEME-NAME.
Load the theme temporarily if needed, extract colors, then unload."
  (let ((colors '())
        (was-loaded (custom-theme-p theme-name))
        (old-themes (custom-available-themes)))
    ;; Disable all currently enabled themes to get clean extraction
    (mapc #'disable-theme custom-enabled-themes)
    
    ;; Load theme freshly
    (load-theme theme-name t nil)
    
    ;; Get colors from theme faces
    (let ((default-face (face-attribute 'default :background nil theme-name))
          (fg (face-attribute 'default :foreground nil theme-name))
          (cursor-face (face-attribute 'cursor :background nil theme-name)))
      
      ;; Extract background and foreground
      (when (stringp default-face)
        (push (cons "bg" (theme-to-pywal--color-to-hex default-face)) colors))
      (when (stringp fg)
        (push (cons "fg" (theme-to-pywal--color-to-hex fg)) colors))
      (when (stringp cursor-face)
        (push (cons "cursor" (theme-to-pywal--color-to-hex cursor-face)) colors))
      
      ;; Try to extract ANSI colors first (if theme defines them)
      (dolist (ansi-mapping '((ansi-color-black . "black")
                              (ansi-color-red . "red")
                              (ansi-color-green . "green")
                              (ansi-color-yellow . "yellow")
                              (ansi-color-blue . "blue")
                              (ansi-color-magenta . "magenta")
                              (ansi-color-cyan . "cyan")
                              (ansi-color-white . "white")
                              (ansi-color-bright-black . "grey")
                              (ansi-color-bright-red . "bright-red")
                              (ansi-color-bright-green . "teal")
                              (ansi-color-bright-yellow . "bright-yellow")
                              (ansi-color-bright-blue . "bright-blue")
                              (ansi-color-bright-magenta . "violet")
                              (ansi-color-bright-cyan . "bright-cyan")
                              (ansi-color-bright-white . "bright-white")))
        (let* ((face (car ansi-mapping))
               (color-name (cdr ansi-mapping))
               (fg-color (face-attribute face :foreground nil theme-name)))
          (when (stringp fg-color)
            (let ((hex-color (theme-to-pywal--color-to-hex fg-color)))
              (when (string-match "^#[0-9a-fA-F]\\{6\\}$" hex-color)
                (push (cons color-name hex-color) colors))))))
      
      ;; Fallback to font-lock colors if ANSI colors not found
      (unless (assoc "red" colors)
        (dolist (face-mapping '((font-lock-comment-face . "grey")
                                (font-lock-string-face . "green")
                                (font-lock-keyword-face . "blue")
                                (font-lock-builtin-face . "cyan")
                                (font-lock-function-name-face . "yellow")
                                (font-lock-variable-name-face . "orange")
                                (font-lock-type-face . "magenta")
                                (font-lock-constant-face . "violet")
                                (font-lock-warning-face . "red")
                                (error . "red")
                                (success . "green")
                                (warning . "yellow")))
          (let* ((face (car face-mapping))
                 (color-name (cdr face-mapping))
                 (fg-color (face-attribute face :foreground nil theme-name)))
            (when (and (stringp fg-color)
                       (string-match "^#[0-9a-fA-F]\\{6\\}$" fg-color))
              (unless (assoc color-name colors)
                (push (cons color-name fg-color) colors)))))))
    
    ;; Disable the theme we loaded
    (disable-theme theme-name)
    
    ;; Re-enable the originally active theme
    (when (boundp 'doom-theme)
      (load-theme doom-theme t nil))
    
    colors))

(defun theme-to-pywal--colors-to-pywal-json (colors theme-name)
  "Convert COLORS alist to pywal JSON format.
THEME-NAME is used in the output filename.
COLORS should be an alist of (name . hex-value)."
  (let* ((bg (or (cdr (assoc "bg" colors))
                 (cdr (assoc "background" colors))
                 "#000000"))
         (fg (or (cdr (assoc "fg" colors))
                 (cdr (assoc "foreground" colors))
                 "#ffffff"))
         (cursor (or (cdr (assoc "cursor" colors))
                     (cdr (assoc "cyan" colors))
                     fg))
         (color0 bg) ; Usually bg
         (color1 (or (cdr (assoc "red" colors)) "#ff0000"))
         (color2 (or (cdr (assoc "green" colors)) "#00ff00"))
         (color3 (or (cdr (assoc "yellow" colors)) "#ffff00"))
         (color4 (or (cdr (assoc "blue" colors)) "#0000ff"))
         (color5 (or (cdr (assoc "magenta" colors)) "#ff00ff"))
         (color6 (or (cdr (assoc "cyan" colors)) "#00ffff"))
         (color7 fg) ; Usually fg
         (color8 (or (cdr (assoc "grey" colors))
                     (cdr (assoc "gray" colors))
                     "#888888"))
         (color9 (or (cdr (assoc "orange" colors))
                     (cdr (assoc "bright-red" colors))
                     color1))
         (color10 (or (cdr (assoc "teal" colors))
                      (cdr (assoc "bright-green" colors))
                      color2))
         (color11 (or (cdr (assoc "bright-yellow" colors)) color3))
         (color12 (or (cdr (assoc "dark-blue" colors))
                      (cdr (assoc "bright-blue" colors))
                      color4))
         (color13 (or (cdr (assoc "violet" colors))
                      (cdr (assoc "bright-magenta" colors))
                      color5))
         (color14 (or (cdr (assoc "dark-cyan" colors))
                      (cdr (assoc "bright-cyan" colors))
                      color6))
         (color15 (or (cdr (assoc "base8" colors))
                      (cdr (assoc "bright-white" colors))
                      fg)))
    
    `((wallpaper . "None")
      (alpha . "100")
      (special . ((background . ,bg)
                  (foreground . ,fg)
                  (cursor . ,cursor)))
      (colors . ((color0 . ,color0)
                 (color1 . ,color1)
                 (color2 . ,color2)
                 (color3 . ,color3)
                 (color4 . ,color4)
                 (color5 . ,color5)
                 (color6 . ,color6)
                 (color7 . ,color7)
                 (color8 . ,color8)
                 (color9 . ,color9)
                 (color10 . ,color10)
                 (color11 . ,color11)
                 (color12 . ,color12)
                 (color13 . ,color13)
                 (color14 . ,color14)
                 (color15 . ,color15))))))

(defun theme-to-pywal-generate-json (theme-file-or-name output-dir)
  "Generate pywal JSON from THEME-FILE-OR-NAME to OUTPUT-DIR.
If THEME-FILE-OR-NAME is a file path, extract from doom-theme file.
If it's a symbol/string, load the theme and extract colors."
  (interactive
   (list (read-file-name "Theme file (or theme name): ")
         (read-directory-name "Output directory: " "~/.config/wal/colorschemes/dark/")))
  
  (let* ((theme-name-str (if (symbolp theme-file-or-name)
                             (symbol-name theme-file-or-name)
                           theme-file-or-name))
         (is-file (and (stringp theme-name-str) (file-exists-p theme-name-str)))
         (theme-name (if is-file
                         (file-name-base theme-name-str)
                       theme-name-str))
         (colors (if is-file
                     (theme-to-pywal--extract-doom-theme-colors theme-name-str)
                   (theme-to-pywal--extract-standard-theme-colors
                    (if (symbolp theme-file-or-name)
                        theme-file-or-name
                      (intern theme-name-str)))))
         (json-data (theme-to-pywal--colors-to-pywal-json colors theme-name))
         (output-file (expand-file-name (concat theme-name ".json") output-dir)))
    
    ;; Write JSON file
    (with-temp-file output-file
      (insert (json-encode json-data))
      (json-pretty-print-buffer))
    
    (message "Generated: %s" output-file)
    output-file))

(defun theme-to-pywal-batch-generate-doom-themes ()
  "Generate pywal JSON for all doom themes."
  (interactive)
  (let* ((themes-dir (expand-file-name ".local/straight/repos/themes/themes/" doom-emacs-dir))
         (theme-files (directory-files themes-dir t "doom-.*-theme\\.el$"))
         (output-dir "~/.config/wal/colorschemes/dark/"))
    (dolist (theme-file theme-files)
      (condition-case err
          (theme-to-pywal-generate-json theme-file output-dir)
        (error (message "Error processing %s: %s" theme-file err))))))

(defun theme-to-pywal-batch-generate-builtin-themes ()
  "Generate pywal JSON for selected built-in Emacs themes."
  (interactive)
  (let ((themes '(wombat leuven leuven-dark tango tango-dark
                  misterioso wheatgrass whiteboard adwaita
                  deeper-blue dichromacy light-blue manoj-dark
                  tsdh-dark tsdh-light))
        (output-dir "~/.config/wal/colorschemes/dark/"))
    (dolist (theme themes)
      (condition-case err
          (theme-to-pywal-generate-json theme output-dir)
        (error (message "Error processing %s: %s" theme err))))))

(defun theme-to-pywal-generate-melpa-theme (theme-name)
  "Generate pywal JSON for a MELPA theme by name."
  (interactive "STheme name: ")
  (theme-to-pywal-generate-json theme-name "~/.config/wal/colorschemes/dark/"))

(provide 'theme-to-pywal)
;;; theme-to-pywal.el ends here
