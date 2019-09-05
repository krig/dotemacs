;;; package --- Set font and theme depending on computer/screen/etc.
;;; Commentary:

(require 'cl)

;;; Code:

;; set font
(defun krig-set-font ()
  "Function to set font depending on system."
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
      (let ((font (font-on-linux)))
        (message "Setting font: %s" font)
        (set-frame-font font nil t)))))

(defun krig-paren-clr (n)
  "Generate grayscale color code.  N is a number between 1 and 9."
  (let ((c (+ ?\x69 (* (1- n) 8))))
    (format "#%X%X%X" c c c)))

(defun krig-rainbow-face-n (n)
  "Format name of face for rainbow-delimiters.  N is number between 1 and 9."
  (intern (format "rainbow-delimiters-depth-%d-face" n)))

(defun krig-rainbow-delimiters ()
  "Set delimiter color depending on nesting depth."
  (add-to-list 'load-path "~/.emacs.d/tools/rainbow-delimiters")
  (progn
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
    (cl-loop for i from 1 to 9 do
             (set-face-foreground (krig-rainbow-face-n i)
                                  (krig-paren-clr i)))))

(defun krig-rainbow-delimiters-noface ()
  "Set delimiter color depending on nesting depth."
  (add-to-list 'load-path "~/.emacs.d/tools/rainbow-delimiters")
  (progn
    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)))


;; set theme
(if (display-graphic-p)
    (progn
      (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
      (load-theme 'nord)
      (krig-rainbow-delimiters)
      (krig-set-font)
      (add-hook 'after-init-hook
                '(lambda ()
                   (message "setting theme for graphical mode")
                   (scroll-bar-mode -1)
                   (menu-bar-mode -1))))
  (add-hook 'after-init-hook
            '(lambda ()
               (message "setting theme for terminal mode")
               (menu-bar-mode -1)
               (set-face-background 'notmuch-message-summary-face nil))))

;;; set-theme.el ends here
