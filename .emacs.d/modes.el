(defun my-tab-fix ()
  (setq show-trailing-whitespace t)
  (smart-tab-mode)
  (smart-tab-mode-on))
;;  (local-set-key [tab] 'indent-or-expand))

;; newline-and-indent
(defun my-set-newline-and-indent()
  (local-set-key [return] 'newline-and-indent))

(defun my-c-style-fix ()
  (c-set-style "bsd")
  (setq indent-tabs-mode nil))

(defun my-java-style-fix ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t))


(dolist (hook '(
		c-mode-hook
		c++-mode-hook
		sh-mode-hook
		emacs-lisp-mode-hook
		lisp-mode-hook
		lisp-interaction-mode-hook
		scheme-mode-hook
		perl-mode-hook
		vala-mode-hook
		ruby-mode-hook
		csharp-mode-hook
		java-mode-hook
		objc-mode-hook
		))
  (add-hook hook 'my-tab-fix)
  (add-hook hook 'my-set-newline-and-indent))

(add-hook 'c-mode-hook 'my-c-style-fix)
(add-hook 'c++-mode-hook 'my-c-style-fix)
(add-hook 'objc-mode-hook 'my-c-style-fix)
(add-hook 'java-mode-hook 'my-java-style-fix)

(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;;(add-to-list 'load-path "~/.emacs.d/ocaml-mode")

;;(setq auto-mode-alist
;;	(cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

;;(if window-system (require 'caml-font))

;; LUA MODE
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; VALA MODE
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; CSHARP MODE
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; MARKDOWN MODE
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(autoload 'markdown-mode "markdown-mode" "Markdown editing mode." t)

;; PYTHON !
(add-to-list 'interpreter-mode-alist '("python2.5" . python-mode))

(defun mypy-indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (hippie-expand arg)
    (python-indent-line)))

(defun mypy-extra-stuff ()
  ;;(local-set-key [tab] 'mypy-indent-or-expand)
  (smart-tab-mode)
  (smart-tab-mode-on)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil))
;;(which-func-mode t))

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook 'mypy-extra-stuff)

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

;; FLYMAKE FOR PYTHON

;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;			  'flymake-create-temp-inplace))
;;	      (local-file (file-relative-name
;;			   temp-file
;;			   (file-name-directory buffer-file-name))))
;;	 (list "epylint" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;		  '("\\.py\\'" flymake-pylint-init)))
;;(add-hook 'python-mode-hook 'flymake-mode)

;; code checking via flymake
;; set code checker here from "epylint", "pyflakes"
(setq pycodechecker "pyflakes")
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pycodecheck-init)))
;;(add-hook 'python-mode-hook 'flymake-mode)

;; RUBY
;;(add-to-load-path (expand-file-name "~/.emacs.d/ruby"))
;;(add-to-list 'load-path "~/.emacs.d/ruby")

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)))
;; If you have Emacs 19.2x or older, use rubydb2x
(autoload 'rubydb "rubydb3x" "Ruby debugger" t)
;; uncomment the next line if you want syntax highlighting
(add-hook 'ruby-mode-hook 'turn-on-font-lock)

;; Flymake hack to determine flymake mode
;; based on buffer major mode, not just by extension

(defcustom flymake-allowed-major-modes
  '((perl-mode flymake-perl-init)
    (python-mode flymake-pycodecheck-init)
    (c-mode flymake-simple-make-init)
    (c++-mode flymake-simple-make-init)
    (objc-mode flymake-simple-make-init)
    )
  "*Major modes syntax checking is allowed for."
  :group 'flymake
  :type '(repeat (string symbol symbol symbol)))

(defun flymake-get-major-mode-and-masks ()
  (let ((fnm flymake-allowed-major-modes)
	(mode-and-masks nil))
    (while (and (not mode-and-masks) fnm)
      (if (eq (car (car fnm)) major-mode)
	  (setq mode-and-masks (cdr (car fnm))))
      (setq fnm (cdr fnm)))
    mode-and-masks))

(defun flymake-get-file-name-mode-and-masks (file-name)
  "Return the corresponding entry from `flymake-allowed-file-name-masks'."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let ((fnm flymake-allowed-file-name-masks)
	(mode-and-masks	 nil))
    (while (and (not mode-and-masks) fnm)
      (if (string-match (car (car fnm)) file-name)
	  (setq mode-and-masks (cdr (car fnm))))
      (setq fnm (cdr fnm)))
    ;; this if added by ~krig
    (if (eq mode-and-masks nil)
	(setq mode-and-masks (flymake-get-major-mode-and-masks)))
    (flymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
    mode-and-masks))


;; RAGEL MODE
(defvar ragel-mode-font-lock-keywords
  '(("\\('.*'\\)" 1 'font-lock-string-face)
    ("\\(\\\".*\\\"\\)" 1 'font-lock-string-face)))

(define-derived-mode ragel-mode fundamental-mode "Ragel"
  "Major mode to edit ragel files."
  (set (make-local-variable 'font-lock-keywords)
       '(ragel-mode-font-lock-keywords)))


;; IMPROVED JAVASCRIPT MODE
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))


;; SCALA MODE
(require 'scala-mode-auto)
(dolist (hook '(scala-mode-hook))
  (add-hook hook 'my-tab-fix)
  (add-hook hook 'my-set-newline-and-indent))


;; GOOGLE GO MODE
(require 'go-mode-load)

(dolist (hook '(
		go-mode-hook
		))
  (add-hook hook 'my-tab-fix)
  (add-hook hook 'my-set-newline-and-indent))

;;;; QUACK
(require 'quack)
(setq quack-fontify-style 'emacs)

;; SLIME
;;(when (eq system-type 'windows-nt)
;; (add-to-list 'load-path "~/.emacs.d/slime/")
;; (setq inferior-lisp-program "c:/Program/clisp-2.45/full/lisp.exe -B c:/Program/clisp-2.45/full -M c:/Program/clisp-2.45/full/lispinit.mem -ansi -q")
;; (require 'slime)
;; (slime-setup))

;;(when (not (eq system-type 'windows-nt))
;;  (setq inferior-lisp-program "sbcl")
;;  (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
;;  (require 'slime))
;;  (slime-setup))

;;(defun cliki:start-slime ()
;;  (unless (slime-connected-p)
;;    (save-excursion (slime))))
;;
;;(add-hook 'slime-mode-hook 'cliki:start-slime)



;; ARC
;; Arc support
(autoload 'run-arc "inferior-arc"
"Run an inferior Arc process, input and output via buffer *arc*.")
(autoload 'arc-mode "arc"
"Major mode for editing Arc." t)
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))
(setq arc-program-name "~/build/arc/arc.sh")
(add-hook 'inferior-arc-mode-hook
	  (lambda ()
	    (set (make-local-variable 'comint-use-prompt-regexp) t)
	    (set (make-local-variable 'comint-prompt-read-only) t)))
