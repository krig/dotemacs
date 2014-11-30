;; loads and sets the theme

;; set font
(when (display-graphic-p)
  (when (krig-macp)
    (setq mac-allow-anti-aliasing t)
    (if (> (display-pixel-width) 1900)
        (set-frame-font "Inconsolata-15")
      (set-frame-font "Inconsolata-13"))
                                        ; nicer lambdas
    (set-fontset-font "fontset-default"
                      'greek-iso8859-7
                      '("Consolas" . "iso10646-1")))

  (when (krig-linuxp)
    (set-frame-font
     (pcase hostname
       ("walker.kri.gs" "Ubuntu Mono-19.5")
       ("kowloon" "Ubuntu Mono-17")
       ("krigpad.site" "Ubuntu Mono-14")
       ("ultralix" "Ubuntu Mono-13.5")
       (_ "Ubuntu Mono-14")))
    (menu-bar-mode -1))

  ;; use the after-init-hook to work around a scrollbar bug..
  (add-hook 'after-init-hook
            '(lambda ()
               (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
               (load-theme 'noctilux)
               (scroll-bar-mode -1)
               ;; rainbow-delimiters
               (when (display-graphic-p)
                 (add-to-list 'load-path "~/.emacs.d/tools/rainbow-delimiters")
                 (progn
                   (defun krig-paren-clr (n)
                     (let ((c (+ ?\x69 (* (1- n) 8))))
                       (format "#%X%X%X" c c c)))

                   (defun krig-rainbow-face-n (n)
                     (intern (format "rainbow-delimiters-depth-%d-face" n)))

                   (require 'rainbow-delimiters)
                   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
                   (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
                   (cl-loop for i from 1 to 9 do
                            (set-face-foreground (krig-rainbow-face-n i)
                                                 (krig-paren-clr i)))))
               )))

(unless (display-graphic-p)
  (menu-bar-mode -1))
