;; programming language modes

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "linux")

;; some generic modes
(require 'generic-x)


;; LORD PROGRAMMATON
(define-generic-mode 'programmaton-mode
  '("#")
  '("def" "if" "case" "elif" "else" "for" "in" "unless" "while" "do" "end" "fn" "throw" "int" "int64" "string" "real" "type")
  '(("[0-9]+" . 'font-lock-variable-name-face)
    ("[\(\)]" . 'paren-face)
    ("\\\"[^\\\"]*\\\"" . 'font-lock-string-face)
    ("['][^']*[']" . 'font-lock-string-face))
  '("\\.lp$")
  '(smart-tab-mode)
  "A mode for lord programmaton files")

;; crmsh
(define-generic-mode 'crmsh-mode
  '("#")
  '("node" "primitive" "group" "clone" "master" "ms"
    "location" "colocation" "order" "rsc_ticket" "rsc_template"
    "property" "rsc_defaults" "op_defaults" "user" "role"
    "fencing_topology" "tag")
  '(("[A-Za-z_][A-Za-z0-9_-]+" . 'font-lock-variable-name-face)
    ("[0-9]+" . 'font-lock-constant-face)
    ("[\(\)]" . 'paren-face)
    ("\\\"[^\\\"]*\\\"" . 'font-lock-string-face)
    ("['][^']*[']" . 'font-lock-string-face))
  '("\\.crm$")
  '(smart-tab-mode)
  "A mode for crmsh")


(defun krig-sh-mode-hook ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (turn-on-fic-mode)
  (setq sh-indent-comment t)
  (local-set-key [return] 'newline-and-indent))
(add-hook 'sh-mode-hook 'krig-sh-mode-hook)

;;;; SH-MODE HACK
;; hack to fix sh-mode heredoc
(defun sh--inside-noncommand-expression (pos)
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (when (nth 1 ppss)
        (goto-char (nth 1 ppss))
        (or
         (pcase (char-after)
           ;; ((...)) or $((...)) or $[...] or ${...}. Nested
           ;; parenthesis can occur inside the first of these forms, so
           ;; parse backward recursively.
           (`?\( (eq ?\( (char-before)))
           ((or `?\{ `?\[) (eq ?\$ (char-before))))
         (sh--inside-noncommand-expression (1- (point))))))))

(defun sh-font-lock-open-heredoc (start string eol)
  "Determine the syntax of the \\n after a <<EOF.
START is the position of <<.
STRING is the actual word used as delimiter (e.g. \"EOF\").
INDENTED is non-nil if the here document's content (and the EOF mark) can
be indented (i.e. a <<- was used rather than just <<).
Point is at the beginning of the next line."
  (unless (or (memq (char-before start) '(?< ?>))
	      (sh-in-comment-or-string start)
              (sh--inside-noncommand-expression start))
    ;; We're looking at <<STRING, so we add "^STRING$" to the syntactic
    ;; font-lock keywords to detect the end of this here document.
    (let ((str (replace-regexp-in-string "['\"]" "" string))
          (ppss (save-excursion (syntax-ppss eol))))
      (if (nth 4 ppss)
          ;; The \n not only starts the heredoc but also closes a comment.
          ;; Let's close the comment just before the \n.
          (put-text-property (1- eol) eol 'syntax-table '(12))) ;">"
      (if (or (nth 5 ppss) (> (count-lines start eol) 1))
          ;; If the sh-escaped-line-re part of sh-here-doc-open-re has matched
          ;; several lines, make sure we refontify them together.
          ;; Furthermore, if (nth 5 ppss) is non-nil (i.e. the \n is
          ;; escaped), it means the right \n is actually further down.
          ;; Don't bother fixing it now, but place a multiline property so
          ;; that when jit-lock-context-* refontifies the rest of the
          ;; buffer, it also refontifies the current line with it.
          (put-text-property start (1+ eol) 'syntax-multiline t))
      (put-text-property eol (1+ eol) 'sh-here-doc-marker str)
      (prog1 sh-here-doc-syntax
        (goto-char (+ 2 start))))))

;; END SH-MODE HACK

(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char-untabify))))))

(defun backward-delete-char-hungry (arg &optional killp)
  "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and
    `backward-delete-char-untabify-method' for details."
  (interactive "*p\nP")
  (let ((backward-delete-char-untabify-method 'hungry))
    (backward-delete-char-untabify arg killp)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun kernel-style-mode-hook ()
  "Add kernel style"
  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
              (arglist-cont-nonempty
               c-lineup-gcc-asm-reg
               c-lineup-arglist-tabs-only)))))

(defun* get-closest-pathname (&optional (file ".git"))
  "Determine the pathname of the first instance of FILE starting from
the current directory towards root. This may not do the correct thing
in presence of links. If it does not find FILE, then it shall return
the name of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

(add-hook 'c-mode-common-hook 'kernel-style-mode-hook)

(require 'compile)

(defun krig-cc-mode-hook ()
  (setq show-trailing-whitespace t)
  (turn-on-fic-mode)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent)
  (subword-mode 1)
  (setq tab-width 8)
  (setq c-basic-offset 8)
  (setq indent-tabs-mode t)
  (c-set-style "linux")

  (setq compilation-read-command nil)
  (set (make-local-variable 'compile-command)
       (format "cd %s && ./build" (file-name-directory (get-closest-pathname))))
  (local-set-key (kbd "C-x SPC") 'compile))

(defun krig-mode-hook ()
  (setq show-trailing-whitespace t)
  (turn-on-fic-mode)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent)
  (subword-mode 1))

(dolist (hook '(
                emacs-lisp-mode-hook
                lisp-mode-hook
                clojure-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                perl-mode-hook
                vala-mode-hook
                ruby-mode-hook
                csharp-mode-hook
                java-mode-hook
                objc-mode-hook
                ))
  (add-hook hook 'krig-mode-hook))

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'krig-cc-mode-hook))


(add-hook 'align-load-hook (lambda ()
       (add-to-list 'align-rules-list
                    '(text-column-whitespace
                      (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                      (group   . 2)
                      (modes   . align-text-modes)
                      (repeat  . t)))))



;; java!
(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))



;; markdown!
(progn
  (add-to-list 'load-path "~/.emacs.d/modes/markdown-mode")
  (add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdwn$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
  (require 'markdown-mode)
  (defun md-extra-stuff ()
    (auto-fill-mode 1))
  (add-hook 'markdown-mode-hook 'md-extra-stuff))



;; python!
(progn
  (add-to-list 'interpreter-mode-alist '("python2.5" . python-mode))
  (defun mypy-extra-stuff ()
    (setq show-trailing-whitespace t)
    (setq tab-width 4)
    (setq indent-tabs-mode nil)
    (define-key python-mode-map "\C-m" 'newline-and-indent))
  (add-hook 'python-mode-hook 'mypy-extra-stuff))


;;;; `Cython' mode.
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(define-derived-mode cython-mode python-mode "Cython"
  (font-lock-add-keywords
   nil
   `((,(concat "\\<\\(NULL"
               "\\|c\\(def\\|har\\|typedef\\)"
               "\\|e\\(num\\|xtern\\)"
               "\\|float"
               "\\|in\\(clude\\|t\\)"
               "\\|object\\|public\\|struct\\|type\\|union\\|void"
               "\\)\\>")
      1 font-lock-keyword-face t))))


(defun next-error-or-flymake ()
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (flymake-goto-next-error)
    (next-error)))


;; flymake!
(progn
  (require 'flymake)
  (setq flymake-gui-warnings-enabled nil)
  (setq help-at-pt-timer-delay 0.9)
  (setq help-at-pt-display-when-idle '(flymake-overlay))
  (global-set-key (kbd "M-n") 'next-error-or-flymake)


  (add-to-list 'load-path "~/.emacs.d/modes/flymake-easy/")
  (add-to-list 'load-path "~/.emacs.d/modes/flymake-python-pyflakes/")
  (when (require 'flymake-python-pyflakes nil 'noerror)
    (setq flymake-python-pyflakes-executable "flake8")
    (setq flymake-python-pyflakes-extra-arguments '("--max-complexity=10" "--max-line-length=99"))
    (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))
  ;; Forces flymake to underline bad lines, instead of fully
  ;; highlighting them; remove this if you prefer full highlighting.
  (custom-set-faces
   '(flymake-errline ((((class color)) (:underline "#aa3333"))))
   '(flymake-warnline ((((class color)) (:underline "#aa4444"))))))


;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 3))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))


;; lisp modes

(progn
  (add-to-list 'load-path "~/.emacs.d/tools/slime")
  (require 'slime-autoloads)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))


