;;; package --- Set font and theme depending on computer/screen/etc.
;;; Commentary:

(require 'cl)

;;; Code:

(defun font-size-for-machine (fontname)
  "Set font size depending on hostname and screen size.  FONTNAME is name of font."
   (pcase (car (split-string *hostname* "\\."))
     ("walker" 15)
     ("kowloon" 17)
     ("krigpad" (if (> (display-pixel-width) 1900) 13 12))
     ("ultralix" 14)
     (_ 14)))

(defun font-candidate (&rest fonts)
  "Return existing font which first match.  FONTS is a list of font names."
  (let ((fonts (map 'list (lambda (f) (format "%s-%d:weight=normal" f (font-size-for-machine f))) fonts)))
    (find-if (lambda (f) (find-font (font-spec :name f))) fonts)))

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
      (let ((font (font-candidate
                   "iosevka Term SS05 Medium"
                   "Input"
                   "Inconsolata"
                   "mononoki"
                   "Ubuntu Mono"
                   "DejaVu Sans Mono"
                   "Fantasque Sans Mono"
                   "Consolas"
                   "Liberation Mono")))
        (message "Setting font: %s" font)
        (set-face-attribute 'default nil :font font)
        (set-face-attribute 'fixed-pitch nil :family nil :inherit 'default)
        (set-face-attribute 'fixed-pitch-serif nil :family nil :inherit 'default)
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

;; set theme
(if (display-graphic-p)
    (add-hook 'after-init-hook
              '(lambda ()
                 (message "setting theme for graphical mode")
                 (scroll-bar-mode -1)
                 (menu-bar-mode -1)
                 (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
                 (load-theme 'atom-one-dark t)
                 (krig-rainbow-delimiters)
                 (krig-set-font)))
  (add-hook 'after-init-hook
            '(lambda ()
               (menu-bar-mode -1)
               (set-face-background 'notmuch-message-summary-face nil))))

;;; set-theme.el ends here
