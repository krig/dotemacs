;; loads and sets the theme

(require 'cl)

(defun font-size-for-machine ()
   (pcase (car (split-string hostname "\\."))
     ("walker" 15)
     ("kowloon" 17)
     ("krigpad" (if (> (display-pixel-width) 1900) 13 13))
     ("ultralix" 15)
     (_ 14)))

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (let ((fonts (map 'list (lambda (f) (format "%s-%d:weight=normal" f (font-size-for-machine))) fonts)))
    (find-if (lambda (f) (find-font (font-spec :name f))) fonts)))

;; set font
(when (display-graphic-p)
  (when (krig-macp)
    (setq mac-allow-anti-aliasing t)
    (set-frame-font (format "Inconsolata-%d" (if (> (display-pixel-width) 1900) 15 13)))
    (set-fontset-font "fontset-default"
                      'greek-iso8859-7
                      '("Consolas" . "iso10646-1")))

  (when (krig-winp)
    (set-frame-font "Consolas-12"))

  (when (krig-linuxp)
    (let ((font (font-candidate
                 "mononoki"
                 "Ubuntu Mono"
                 "Input"
                 "DejaVu Sans Mono"
                 "Inconsolata"
                 "Fantasque Sans Mono"
                 "Consolas"
                 "Liberation Mono")))
      (message "Setting font: %s" font)
      (set-face-attribute 'default nil :font font)))
  (menu-bar-mode -1))

;; work around the scrollbar bug
(when (display-graphic-p)
  (add-hook 'after-init-hook
            '(lambda () (scroll-bar-mode -1))))

(defun krig-paren-clr (n)
  (let ((c (+ ?\x69 (* (1- n) 8))))
    (format "#%X%X%X" c c c)))

(defun krig-rainbow-face-n (n)
  (intern (format "rainbow-delimiters-depth-%d-face" n)))

;; set theme
(when (display-graphic-p)
  (add-hook 'after-init-hook
            '(lambda ()
               (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
               ;;(load-theme 'krig)
               (load-theme 'misterioso)
               ;;(load-theme 'noctilux)
               ;;(load-theme 'proctologist)
               ;;(load-theme 'adwaita)
               (load-theme 'misterioso)
               ;; rainbow-delimiters
               (add-to-list 'load-path "~/.emacs.d/tools/rainbow-delimiters")
               (progn
                 (require 'rainbow-delimiters)
                 (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
                 (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
                 (cl-loop for i from 1 to 9 do
                          (set-face-foreground (krig-rainbow-face-n i)
                                               (krig-paren-clr i)))))))

(unless (display-graphic-p)
  (menu-bar-mode -1))
