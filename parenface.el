;;; parenface.el --- Provide a face for parens in lisp modes.
;; By Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $

;; Add a paren-face to emacs and add support for it to the various lisp modes.
;;
;; Based on some code that Boris Schaefer <boris@uncommon-sense.net> posted
;; to comp.lang.scheme in message <87hf8g9nw5.fsf@qiwi.uncommon-sense.net>.
;; [poo pee poo]
;; {curly moe}

(defvar parenface-paren-face 'parenface-paren-face)
(defvar parenface-bracket-face 'parenface-bracket-face)
(defvar parenface-curly-face 'parenface-curly-face)

(defface parenface-paren-face
    '((((class color))
       (:foreground "DimGray")))
  "Face for displaying a paren."
  :group 'faces)

(defface parenface-bracket-face
    '((((class color))
       (:foreground "DimGray")))
  "Face for displaying a bracket."
  :group 'faces)

(defface parenface-curly-face
    '((((class color))
       (:foreground "DimGray")))
  "Face for displaying a curly brace."
  :group 'faces)

(defmacro paren-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
    (let* ((re0 "(\\|)")
	   (re1 "\\[\\|]")
	   (re2 "{\\|}")
           (match0 (assoc re0 ,keywords))
	   (match1 (assoc re1 ,keywords))
	   (match2 (assoc re2 ,keywords)))
      (unless (eq (cdr match0) parenface-paren-face)
        (setq ,keywords (append (list (cons re0 parenface-paren-face)) ,keywords)))
      (unless (eq (cdr match1) parenface-bracket-face)
	(setq ,keywords (append (list (cons re1 parenface-bracket-face)) ,keywords)))
      (unless (eq (cdr match2) parenface-curly-face)
	(setq ,keywords (append (list (cons re2 parenface-curly-face)) ,keywords))))))

;; Keep the compiler quiet.
(eval-when-compile
  (defvar scheme-font-lock-keywords-2 nil)
  (defvar lisp-font-lock-keywords-2 nil))

;; (add-hook 'scheme-mode-hook           (paren-face-add-support scheme-font-lock-keywords-2))
;; (add-hook 'inferior-scheme-mode-hook  (paren-face-add-support scheme-font-lock-keywords-2))
;; (add-hook 'lisp-mode-hook             (paren-face-add-support lisp-font-lock-keywords-2))
;; (add-hook 'emacs-lisp-mode-hook       (paren-face-add-support lisp-font-lock-keywords-2))
;; (add-hook 'lisp-interaction-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
;; (add-hook 'arc-mode-hook              (paren-face-add-support arc-font-lock-keywords-2))
;; (add-hook 'inferior-arc-mode-hook     (paren-face-add-support arc-font-lock-keywords-2))
;; (add-hook 'clojure-mode-hook          (paren-face-add-support clojure-font-lock-keywords))

(provide 'parenface)

;; parenface.el ends here
