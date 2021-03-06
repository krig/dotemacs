;; programming language modes

(require 'cl-lib)

(add-to-list 'load-path "~/.emacs.d/modes")

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "linux")

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


(cl-defun get-closest-pathname (&optional (file ".git"))
  "Determine the pathname of the first instance of FILE starting from
the current directory towards root. This may not do the correct thing
in presence of links. If it does not find FILE, then it shall return
the name of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (cl-loop
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
  (subword-mode 1)
  (setq tab-width 8)
  (setq c-basic-offset 8)
  (setq indent-tabs-mode t)
  (c-set-style "linux")

  ;;(setq compilation-read-command nil)
  ;; (set (make-local-variable 'compile-command)
  ;;      (let ((projdir (file-name-directory (get-closest-pathname))))
  ;;        (cond
  ;;         ((file-exists-p (format "%s/build" projdir))
  ;;          (format "cd %s && ./build" projdir))
  ;;         ((file-exists-p (format "%s/waf" projdir))
  ;;          (format "cd %s && ./waf" projdir))
  ;;         ((file-exists-p (format "%s/Makefile" projdir))
  ;;          (format "cd %s && make -j" projdir))
  ;;         ((file-exists-p (format "%s/mk" projdir))
  ;;          (format "cd %s && ./mk" projdir))
  ;;         ((file-exists-p (format "%s/Cargo.toml" projdir))
  ;;          (format "cd %s && cargo build" projdir)))))
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent)
  (local-set-key (kbd "M-j") 'join-line))
  ;;;(local-set-key (kbd "C-x SPC") 'compile))

(defun krig-is-go (projdir)
  "Check if project dir contain a go project.  PROJDIR is the project dir."
  (and (not (empty-string-p (getenv "GOPATH"))) (string-prefix-p (car (split-string (getenv "GOPATH") ":")) projdir)))

(defun compile-and-run ()
  "Used by krig-compile-hook to compile, then run."
  (interactive)
  (let* ((projdir (file-name-directory (get-closest-pathname)))
         ;;; Remember to fix krig-compile-hook too!
         (command (cond
                   ((equal major-mode 'ponylang-mode)
                    (format "cd %s && ponyc" projdir))
                   ((krig-is-go projdir)
                    (format "cd %s && go run" projdir))
                   ((file-exists-p (format "%s/Cargo.toml" projdir))
                    (format "cd %s && cargo run" projdir))
                   ((file-exists-p (format "%s/Makefile" projdir))
                    (format "cd %s && make run" projdir))
                   (t
                    (format "cd %s && make run" projdir)))))
    (async-shell-command command)))

(defun krig-compile-hook ()
  "Run the appropriate thingie to compile."
  (setq compilation-read-command nil)
  (set (make-local-variable 'compile-command)
       (let ((projdir (file-name-directory (get-closest-pathname))))
         (cond
          ((equal major-mode 'ponylang-mode)
           (format "cd %s && ponyc" projdir))
          ((krig-is-go projdir)
           (format "cd %s && go test" projdir))
          ((file-exists-p (format "%s/build" projdir))
           (format "cd %s && ./build" projdir))
          ((file-exists-p (format "%s/waf" projdir))
           (format "cd %s && ./waf" projdir))
          ((file-exists-p (format "%s/Makefile" projdir))
           (format "cd %s && make -j" projdir))
          ((file-exists-p (format "%s/mk" projdir))
           (format "cd %s && ./mk" projdir))
          ((file-exists-p (format "%s/Cargo.toml" projdir))
           (format "cd %s && cargo test" projdir))
          (t
           (format "cd %s && make -j" projdir)))))
  (local-set-key (kbd "C-x SPC") 'compile)
  (local-set-key (kbd "C-x r") 'compile-and-run))

(defun krig-mode-hook ()
  (setq show-trailing-whitespace t)
  (turn-on-fic-mode)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent)
  (local-set-key (kbd "M-j") 'join-line)
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

(add-hook 'prog-mode-hook 'krig-compile-hook)

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


;; clean up some modelines
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "js2")

;; javascript!
(defun krig-js2-mode-hook ()
  (setq show-trailing-whitespace t)
  (turn-on-fic-mode)
  (subword-mode 1)
  (setq tab-width 2)
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))
(add-hook 'js2-mode-hook 'krig-js2-mode-hook)
(add-hook 'js-mode-hook 'krig-js2-mode-hook)

;; markdown!
(progn
  (add-to-list 'load-path "~/.emacs.d/modes/markdown-mode")
  (add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdwn$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
  (require 'markdown-mode)
  (defun md-extra-stuff ()
    (setq markdown-header-scaling nil)
    (setq markdown-gfm-use-electric-backquote nil)
    (setq markdown-fontify-code-blocks-natively t)
    (copy-face 'font-lock-constant-face 'markdown-code-face)
    (auto-fill-mode 1))
  (add-hook 'markdown-mode-hook 'md-extra-stuff))

;; ponylang
(progn
  (defun krig-ponylang-mode-hook ()
    (setq show-trailing-whitespace t)
    (turn-on-fic-mode)
    (setq tab-width 2)
    (local-set-key [return] 'newline-and-indent))

  (add-to-list 'load-path "~/.emacs.d/modes/ponylang-mode")
  (add-to-list 'auto-mode-alist '("\\.pony$" . ponylang-mode))
  (autoload 'ponylang-mode "ponylang-mode" nil t)
  (with-eval-after-load 'ponylang-mode
    (add-hook 'ponylang-mode-hook 'krig-ponylang-mode-hook)))

;; toml :P
(progn
  (add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode))
  (require 'toml-mode))


;; yaml!
(progn
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


;; python!
(progn
  (add-to-list 'interpreter-mode-alist '("python2.7" . python-mode))
  (defun mypy-extra-stuff ()
    (setq show-trailing-whitespace t)
    (turn-on-fic-mode)
    (setq tab-width 4)
    (setq indent-tabs-mode nil)
    (defun python-indent-with-tabs ()
      (interactive)
      (setq tab-width 4)
      (setq indent-tabs-mode t)
      (setq python-indent-offset 4))
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


(defun next-error-or-flycheck ()
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (flycheck-next-error)
    (next-error)))

;; flycheck!
(progn
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe)
  ;;(setq flycheck-clang-language-standard "c++11")
  (setq flycheck-flake8-maximum-line-length 160)

  ;; replace ugly indicator
  (add-hook 'flycheck-mode-hook
            (lambda ()
              ;; Assume python 3
              (if (save-excursion
                      (goto-char 1)
                      (not (looking-at "#!/usr/bin/python2")))
                  (progn
                    (setq flycheck-python-flake8-executable "flake8-3.5")
                    (setq flycheck-python-pylint-executable "pylint-3.5")
                    (setq flycheck-python-pycompile-executable "python3"))
                (progn
                  (setq flycheck-python-flake8-executable "flake8-2.7")
                  (setq flycheck-python-pylint-executable "pylint-2.7")
                  (setq flycheck-python-pycompile-executable "python2.7")))
              (when (fboundp 'define-fringe-bitmap)
                (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
                  (vector #b00000100
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00001110
                          #b00000100
                          #b00000000)))))

  (global-set-key (kbd "M-n") 'next-error-or-flycheck))

;; flymake!
;; (defun next-error-or-flymake ()
;;   (interactive)
;;   (if (bound-and-true-p flymake-mode)
;;       (flymake-goto-next-error)
;;     (next-error)))
;; (progn
;;   (require 'flymake)
;;   (setq flymake-gui-warnings-enabled nil)
;;   (setq help-at-pt-timer-delay 0.9)
;;   (setq help-at-pt-display-when-idle '(flymake-overlay))
;;   (global-set-key (kbd "M-n") 'next-error-or-flymake)


;;   (add-to-list 'load-path "~/.emacs.d/modes/flymake-easy/")
;;   (add-to-list 'load-path "~/.emacs.d/modes/flymake-python-pyflakes/")
;;   (when (require 'flymake-python-pyflakes nil 'noerror)
;;     (setq flymake-python-pyflakes-executable "flake8")
;;     (setq flymake-python-pyflakes-extra-arguments '("--max-complexity=10" "--max-line-length=120"))
;;     (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))
;;   ;; Forces flymake to underline bad lines, instead of fully
;;   ;; highlighting them; remove this if you prefer full highlighting.
;;   (custom-set-faces
;;    '(flymake-errline ((((class color)) (:underline "#aa3333"))))
;;    '(flymake-warnline ((((class color)) (:underline "#aa4444"))))))


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
  (when (not (krig-winp))
    (setq inferior-lisp-program "/usr/bin/sbcl"))
  (setq slime-contribs '(slime-fancy)))


;; auto-complete
(progn
  (add-to-list 'load-path "~/.emacs.d/modes/auto-complete")    ; This may not be appeared if you have already added.
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup))
  ;;(global-auto-complete-mode t))

; nope, I hate global auto-complete-mode
  ;(ac-config-default))

;; rust-mode

(when (not (krig-winp))
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-hook 'rust-mode-hook 'krig-rust-mode-hook))

;; go mode
(progn
  (add-to-list 'load-path "~/.emacs.d/modes/go-mode")
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook
            (lambda ()
              (setq gofmt-args '("-s"))
              )))

;; nasm mode
(progn
  (autoload 'nasm-mode "nasm-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))

;; jai mode
(progn
  (autoload 'jai-mode "jai-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode)))

