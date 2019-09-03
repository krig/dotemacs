;;; soft-theme.el --- Anti-highlighting UI Theme for Emacs

;; Copyright (C) 2015 Kristoffer Gronlund

(require 'hexrgb)

(defun soft-theme-paren-clr (n)
  (let ((c (+ ?\x59 (* (1- n) 8))))
    (format "#%X%X%X" c c c)))

(deftheme soft
  "Anti-highlighting UI Theme for Emacs")

(display-color-cells (selected-frame))

(let* ((class '((class color) (min-colors 89)))
       (256color (eq (display-color-cells (selected-frame)) 256))
       (background "#fefefe")
       (foreground "#333")
       (comment "#434C5E")
       (commentbg "#fefefe")
       (warning "#996611")
       (error "#cc0000"))
  (custom-theme-set-faces
   'soft
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))

   `(font-lock-builtin-face ((,class (:foreground ,foreground))))
   `(font-lock-constant-face ((,class (:foreground ,foreground))))
   `(font-lock-function-name-face ((,class (:foreground ,foreground))))
   `(font-lock-keyword-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,foreground))))
   `(font-lock-preprocessor-face ((,class (:foreground ,foreground))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,foreground))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,foreground))))
   `(font-lock-string-face ((,class (:foreground ,foreground))))
   `(font-lock-type-face ((,class (:foreground ,foreground))))
   `(font-lock-variable-name-face ((,class (:foreground ,foreground))))
   `(font-lock-warning-face ((,class (:foreground ,warning))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :background ,commentbg))))
   `(font-lock-comment-face ((,class (:foreground ,comment :background ,commentbg))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-doc-string-face ((,class (:foreground ,comment))))

   `(shadow ((,class (:foreground ,foreground))))
   `(success ((,class (:foreground ,foreground))))
   `(error ((,class (:foreground ,error))))
   `(warning ((,class (:foreground ,warning))))

   ;; Fly(| check make)
   `(flycheck-error ((,class (:underline (:style wave :color ,error)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,warning)))))
   `(flymake-warnline ((,class (:underline (:style wave :color ,warning) :background ,background))))
   `(flymake-errline ((,class (:underline (:style wave :color ,error) :background ,background))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,(soft-theme-paren-clr 1)))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,(soft-theme-paren-clr 2)))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,(soft-theme-paren-clr 3)))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,(soft-theme-paren-clr 4)))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,(soft-theme-paren-clr 5)))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,(soft-theme-paren-clr 6)))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,(soft-theme-paren-clr 7)))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,(soft-theme-paren-clr 8)))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,(soft-theme-paren-clr 9)))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,error))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'soft)

;;; soft-theme.el ends here
