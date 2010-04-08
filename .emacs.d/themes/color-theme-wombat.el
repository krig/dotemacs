;; Wombat Colour Theme for Emacs.
;;
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/themes/color-theme-wombat.el")
;;
;; And then (color-theme-wombat) to activate it.
;;
;; All patches welcome

(defun color-theme-wombat ()
  "Based on the wombat theme for vim"
  (interactive)
  (color-theme-install
   '(color-theme-wombat
     ((background-color . "#242424")
      (background-mode . dark)
      (border-color . "#141414")
      (cursor-color . "#656565")
      (foreground-color . "#f6f3e8")
      (mouse-color . "sienna1"))
     (default ((t (:background "#242424" :foreground "#f6f3e8"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#242424" :foreground "#F6f3e8"))))
     (border ((t (:inherit default))))
     (fringe ((t (:inherit default))))

     (font-lock-builtin-face ((t (:foreground "#8ac6f2"))))
     (font-lock-comment-face ((t (:italic t :foreground "#99968b"))))
     (font-lock-constant-face ((t (:foreground "#e5786d"))))
     (font-lock-doc-string-face ((t (:italic t :foreground "#95e454"))))
     (font-lock-function-name-face ((t (:foreground "#cae682" :underline nil :weight bold))))
     (font-lock-keyword-face ((t (:foreground "#8ac6f2"))))
     (font-lock-preprocessor-face ((t (:foreground "#e5786d"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-string-face ((t (:italic t :foreground "#95e454"))))
     (font-lock-type-face ((t (:foreground "#eac682"))))
     (font-lock-variable-name-face ((t (:foreground "#cae682"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (flymake-errline ((t (:underline "#cc9393"))))
     (flymake-warnline ((t (:underline "#f0dfaf"))))

     (speedbar-button-face ((t (:foreground "#f6f3e8"))))
     (speedbar-file-face ((t (:foreground "#eac682"))))
     (speedbar-directory-face ((t (:foreground "#cae682"))))
     (speedbar-tag-face ((t (:foreground "#95e454"))))
     (speedbar-selected-face ((t (:foreground "#8ac6f2" :weight bold :underline nil))))
     (speedbar-highlight-face ((t (:background "#444444" :foreground "#e5786d"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#e7f6da"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "#95e454"))))

     (region ((t (:background "#333333"))))

     (tabbar-default-face ((t (:inherit default :family "fixed" :background "#333333" :foreground "#666666" :height 80))))
     (tabbar-button-face ((t (:inherit tabbar-default-face))))
     (tabbar-selected-face ((t (:inherit tabbar-button-face :foreground "#f57b6f"))))
     (tabbar-unselected-face ((t (:inherit tabbar-button-face))))

     (trailing-whitespace ((t (:inherit default :underline "#885030"))))

     (minibuffer-prompt ((t (:foreground "#777777"))))
     (show-paren-mismatch ((t (:background "#ffc0c0" :foreground "#ff2222" :weight bold))))
     (show-paren-match ((t (:background "#857b6f" :foreground "#f6f3e8" :weight bold))))
     (match ((t (:weight bold))))

     (ido-first-match ((t (:inherit font-lock-doc-string-face))))
     (ido-only-match ((t (:inherit font-lock-constant-face))))
     (ido-subdir ((t (:inherit font-lock-type-face))))

     (isearch ((t (:foreground "#f6f3e8" :background "#cf4f00"))))
     (isearch-lazy-highlight ((t (:foreground "#f6f3e8" :background "#222222" :weight normal))))

     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))

     (mode-line-inactive ((t (:background "#222222" :foreground "#444444" :box nil))))
     (mode-line ((t (:background "#333333" :foreground "#f6f3e8" :box nil))))
     (modeline-buffer-id ((t (:inherit mode-line :background "#343030"))))

     (highlight ((t (:background "#222222"))))
     (highlight-face ((t (:background "#222222"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline t))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))

