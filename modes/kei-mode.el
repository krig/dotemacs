;; kei-mode.el - very basic kei mode

(require 'cl)
(require 'rx)
(require 'js)

(defconst kei-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst kei-builtins
  '("cast" "it" "typeof" "sizeof"))

(defconst kei-keywords
  '("if" "else" "elif" "then" "while" "for" "case" "struct" "enum"
    "return" "new" "remove" "continue" "break" "defer" "inline" "try"
    "using", "where"))

(defconst kei-constants
  '("null" "true" "false"))

(defconst kei-typenames
  '("int" "u64" "u32" "u16" "u8"
    "i64" "i32" "i16" "i8"
    "f32" "f64" "str" "bool"
    "maybe" "result"))

(defun kei-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun kei-keywords-rx (keywords)
  "build keyword regexp"
  (kei-wrap-word-rx (regexp-opt keywords t)))

(defconst kei-hat-type-rx (rx (group (and "^" (1+ word)))))
(defconst kei-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst kei-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defconst kei-font-lock-defaults
  `(
    ;; Keywords
    (,(kei-keywords-rx kei-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(kei-keywords-rx kei-builtins) 1 font-lock-variable-name-face)

    ;; Constants
    (,(kei-keywords-rx kei-constants) 1 font-lock-constant-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; At directives
    ("@\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(kei-wrap-word-rx kei-number-rx) . font-lock-constant-face)

    ;; Types
    (,(kei-keywords-rx kei-typenames) 1 font-lock-type-face)
    (,kei-hat-type-rx 1 font-lock-type-face)
    (,kei-dollar-type-rx 1 font-lock-type-face)

    ("---" . font-lock-constant-face)
    ))

;; add setq-local for older emacs versions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defconst kei--defun-rx "\(.*\).*\{")

(defmacro kei-paren-level ()
  `(car (syntax-ppss)))

(defun kei-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at kei--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun kei-beginning-of-defun (&optional count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (kei-paren-level)))
    (while (and
            (not (kei-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (kei-paren-level))
      (while (>= (kei-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (kei-line-is-defun)
      (beginning-of-line)))

(defun kei-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (kei-paren-level)))
    (when (> orig-level 0)
      (kei-beginning-of-defun)
      (end-of-line)
      (setq orig-level (kei-paren-level))
      (skip-chars-forward "^}")
      (while (>= (kei-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'kei-parent-mode
 (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode kei-mode kei-parent-mode "Kei"
 :syntax-table kei-mode-syntax-table
 :group 'kei
 (setq bidi-paragraph-direction 'left-to-right)
 (setq-local require-final-newline mode-require-final-newline)
 (setq-local parse-sexp-ignore-comments t)
 (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
 (setq-local comment-start "/*")
 (setq-local comment-end "*/")
 (setq-local indent-line-function 'js-indent-line)
 (setq-local font-lock-defaults '(kei-font-lock-defaults))
 (setq-local beginning-of-defun-function 'kei-beginning-of-defun)
 (setq-local end-of-defun-function 'kei-end-of-defun)

 (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kei\\'" . kei-mode))

(provide 'kei-mode)
