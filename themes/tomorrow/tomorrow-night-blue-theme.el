;;; tomorrow-night-blue-theme.el --- custom theme for faces

;;; Commentary:
;;
;;; Tomorrow Night Blue Theme
;;
;; Originally by Chris Kempson https://github.com/ChrisKempson/Tomorrow-Theme
;; Ported to GNU Emacs by Chris Charles
;; Ported to GNU Emacs 24's built-in theme system by Jim Myhrberg (@jimeh)

;;; Code:

(deftheme tomorrow-night-blue
  "A Pastel Coloured Theme")

(let ((background "#002451")
      (current-line "#00346e")
      (selection "#003f8e")
      (foreground "#ffffff")
      (comment "#7285b7")
      (cursor "#ffffff")
      (red "#ff9da4")
      (orange "#ffc58f")
      (yellow "#ffeead")
      (green "#d1f1a9")
      (aqua "#99ffff")
      (blue "#bbdaff")
      (purple "#ebbbff"))

  (custom-theme-set-faces
   'tomorrow-night-blue

   ;; Built-in stuff (Emacs 23)
   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background ,current-line))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(mode-line ((t (:background ,current-line :foreground ,foreground))))
   `(region ((t (:background ,selection))))

   ;; Font-lock stuff
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; linum-mode
   `(linum ((t (:background ,current-line :foreground ,foreground))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,current-line))))
   `(org-link ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,red))))

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

   ;; speedbar-mode
   `(speedbar-button-face ((t (:foreground ,green :background ,background))))
   `(speedbar-file-face ((t (:foreground ,blue :background ,background))))
   `(speedbar-directory-face ((t (:foreground ,purple :background ,background))))
   `(speedbar-tag-face ((t (:foreground ,blue :background ,background))))
   `(speedbar-selected-face ((t (:foreground ,orange :background ,current-line :underline nil :weight bold))))
   `(speedbar-highlight-face ((t (:foreground ,yellow :background ,selection))))
   `(speedbar-separator-face ((t (:foreground ,red :background ,background))))

   `(ido-subdir ((t (:foreground ,purple :background ,background))))
   `(ido-only-match ((t (:foreground ,yellow :background ,selection))))
   `(ido-first-match ((t (:foreground ,green :background ,background))))
   )

  (custom-theme-set-variables
   'tomorrow-night-blue

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'tomorrow-night-blue)

;;; tomorrow-night-blue-theme.el ends here
