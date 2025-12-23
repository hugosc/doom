;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups
   '(("org"
      ("Project: brain2"
       (projectile-root "brain2" . "/home/croc/Documents/brain2/")))))
 '(ibuffer-saved-filters
   '(("org" (used-mode . org-mode))
     ("programming"
      (or (derived-mode . prog-mode) (mode . ess-mode) (mode . compilation-mode)))
     ("text document" (and (derived-mode . text-mode) (not (starred-name))))
     ("TeX"
      (or (derived-mode . tex-mode) (mode . latex-mode) (mode . context-mode)
          (mode . ams-tex-mode) (mode . bibtex-mode)))
     ("web"
      (or (derived-mode . sgml-mode) (derived-mode . css-base-mode)
          (derived-mode . js-base-mode) (derived-mode . typescript-ts-base-mode)
          (mode . js2-mode) (derived-mode . haml-mode) (mode . sass-mode)))
     ("gnus"
      (or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode)
          (mode . gnus-summary-mode) (mode . gnus-article-mode)))))
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "LOOP(l)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
      "SKILL(s)" "DONE(d)" "KILL(k)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
     (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
 '(package-selected-packages
   '(ample-theme blackjack cacao-theme darktooth-theme gptel-agent kuronami-theme
     labburn-theme org-pomodoro punpun-themes sublime-themes sunburn-theme
     warm-night-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
