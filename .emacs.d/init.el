;; MISC THINGS FROM EMACS.D
(defun add-to-load-path (path-string)
  (message (format "Passed %S..." path-string))
  (if (stringp path-string)
      (when (file-exists-p path-string)
	(message (format "Adding %S to load-path..." path-string))
	(add-to-list 'load-path (expand-file-name path-string)))
    (add-to-load-path (car path-string))
    (if (cdr path-string)
	(add-to-load-path (cdr path-string)))))

;;(add-to-list 'load-path "~/.emacs.d")
(add-to-load-path (expand-file-name "~/.emacs.d"))
(add-to-load-path (expand-file-name "~/.emacs.d/scala-mode"))
(add-to-load-path (expand-file-name "~/.emacs.d/emacs-eclim"))
(add-to-load-path (expand-file-name "~/.emacs.d/smart-tab"))


;; multi-occur marks all occurrances of regexp (WANT)
;; (global-set-key "\C-z" 'multi-occur)
;; pop-to-mark-command.. i don't quite understand, but sounds useful
;; (global-set-key "\M-j" 'pop-to-mark-command)
;; ..don't really see the need for this
;; (global-set-key "\M-q" 'revert-buffer)

;; COLOR THEME
(when (eq system-type 'windows-nt)
  (set-default-font
   "-*-Consolas-normal-r-normal-normal-13-*-*-*-c-*-iso8859-1"))
;;(add-to-list 'load-path "~/.emacs.d/color-theme")
;;(require 'color-theme)

;;(load-library "color-theme-tango")
;;(load-library "color-theme-tango-light")
;;(color-theme-tango-light)
;;(color-theme-xemacs)
;;(color-theme-tango)

;;(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-load-path (expand-file-name "~/.emacs.d/color-theme"))
(load-library "zenburn")
(color-theme-zenburn)

;;(unless (zenburn-f;ormat-spec-works-p)
;;  (zenburn-define-format-spec))

;;(set-face-foreground 'mode-line "#acbc90")
;;(set-face-background 'mode-line "#353b37")

;; UNTABIFY
(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;;(setq initial-major-mode
;;	(function (lambda ()
;;	  (text-mode)
;;	  (turn-on-auto-fill))))


(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
	bufname)
    (while (progn
	     (setq bufname (concat "*scratch"
				   (if (= n 0) "" (int-to-string n))
				   "*"))
	     (setq n (1+ n))
	     (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (text-mode)
    (turn-on-auto-fill)))
(global-set-key "\C-c\C-b" 'create-scratch-buffer)

;; VARIABLES
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(column-number-mode t)
(global-font-lock-mode t)
(icomplete-mode t)
(auto-compression-mode t)
(setq font-lock-maximum-decoration t)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq transient-mark-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(customize-set-variable 'scroll-bar-mode 'right)
(winner-mode 1)
(global-auto-revert-mode t)
(blink-cursor-mode -1)
(global-set-key "\C-z" 'undo)

(setq-default indent-tabs-mode t)

(require 'tramp)
(setq tramp-completion-reread-directory-timeout nil)
(setq tramp-default-method "scpc")
(add-to-list 'tramp-remote-path "~/bin")

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; BACKUPS
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir "~/.eamcs.d/autosaves/")

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
	  (if buffer-file-name
	      (concat "#" (file-name-nondirectory buffer-file-name) "#")
	    (expand-file-name
	     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))

(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;;(add-to-list 'backup-directory-alist
;;		    (cons "." "~/.emacs.d/backups/"))
;;(setq tramp-backup-directory-alist backup-directory-alist)

(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")

;;(add-to-list 'backup-directory-alist
;;	       (cons tramp-file-name-regexp nil))

;; IBUFFER
;; ibuffer:
;;(require 'ibuffer)
;;(setq ibuffer-default-sorting-mode 'major-mode)
;;(setq ibuffer-always-show-last-buffer t)
;;(setq ibuffer-view-ibuffer t)
;;(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)



;;(require 'color-theme)
;;(color-theme-initialize)

;;(color-theme-black-on-gray)

;;(load-library "color-theme-wombat.el")
;;(color-theme-wombat)

;; show tabs
;;(show-ws-toggle-show-tabs)

;; make tabs a tiny bit darker than the background color
;;(set-face-background 'show-ws-tab "#3a3a3a")

;;(load-library "color-theme-blackboard.el")
;;(color-theme-blackboard)
;;(load-library "color-theme-ir-black.el")
;;(color-theme-ir-black)
;;(load-library "color-theme-railscasts.el")
;;(color-theme-railscasts)

;;(set-face-background 'mode-line "#DDDDDD")
;;(set-face-foreground 'mode-line "#222222")
;;(set-face-background 'mode-line-inactive "#222222")
;;(set-face-foreground 'mode-line-inactive "#AAAAAA")
;;(set-face-background 'modeline-buffer-id "#222222")
;;(set-face-foreground 'modeline-buffer-id "#CCCCCC")

;; C SOURCE FOR EMACS
(when (eq system-type 'windows-nt)
  (setq find-function-C-source-directory "C:/Program/Emacs/src/src"))
(when (not (eq system-type 'windows-nt))
  (setq find-function-C-source-directory "~/.emacs.d/emacs-snapshot-20080228/src"))

;; SERVER
(when (not (eq system-type 'windows-nt))
  (server-start))

;; IDO

(ido-mode t)
(ido-everywhere t)
(setq ido-save-directory-list-file "~/.emacs.d/ido.history")
(setq ido-max-work-file-list 40)
(setq ido-enable-flex-matching t)
(setq confirm-nonexistent-file-or-buffer nil)
(add-hook 'ido-define-mode-map-hook 'ido-my-keys)
(defun ido-my-keys ()
  (define-key ido-mode-map "\t" 'ido-complete)
					;tab is better for completion lists
					;(define-key ido-mode-map (kbd "tab")
					; 'ido-complete)
  (define-key ido-mode-map "\C-t" 'ido-toggle-regexp) ; same as in isearch
  (define-key ido-mode-map "\C-d" 'ido-enter-dired)) ; cool

;; VALA MODE
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; CSHARP MODE
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))


;; TAB EXPANSION FOR C/C++

;;(make-hippie-expand-function
;; '(try-expand-dabbrev-visible
;;   try-expand-dabbrev-from-kill
;;   try-expand-dabbrev-all-buffers
;;   try-complete-file-name-partially
;;   try-complete-file-name))

;;(setq hippie-expand-try-functions-list
;;(fset 'my-hippie (make-hippie-expand-function
;;		    '(try-expand-dabbrev-visible
;;		      try-expand-dabbrev
;;		      try-expand-dabbrev-from-kill
;;		      try-expand-dabbrev-all-buffers
;;		      ;;try-expand-all-abbrevs
;;		      ;;try-expand-list
;;		      ;;try-expand-line
;;		      try-complete-lisp-symbol-partially
;;		      try-complete-lisp-symbol
;;		      try-complete-file-name-partially
;;		      try-complete-file-name)))

(add-to-list 'load-path
	     "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)

(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-complete-file-name-partially
	try-complete-file-name))

;;(global-smart-tab-mode 1)

;;(defun indent-or-expand (arg)
;;  "Either indent according to mode, or expand the word preceding
;;point."
;;  (interactive "*P")
;;  (if (and
;;	 (or (bobp) (= ?w (char-syntax (char-before))))
;;	 (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;	(my-hippie arg)
;;    (indent-according-to-mode)))

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
		))
  (add-hook hook 'my-tab-fix)
  (add-hook hook 'my-set-newline-and-indent))

(add-hook 'c-mode-hook 'my-c-style-fix)
(add-hook 'c++-mode-hook 'my-c-style-fix)
(add-hook 'java-mode-hook 'my-java-style-fix)

(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))


;; (autoload 'paredit-mode "paredit"
;; "Minor mode for pseudo-structurally editing Lisp code."
;; t)
;; (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))

;;(add-to-list 'load-path "~/.emacs.d/ocaml-mode")

;;(setq auto-mode-alist
;;	(cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

;;(if window-system (require 'caml-font))

;;(require 'sr-speedbar)
;;(global-set-key [(super 's)] 'sr-speedbar-toggle)

;; LINE NUMBERS
(require 'linum)

;;(require 'washout)

;; PABBREV
;;(require 'pabbrev)
;;(global-pabbrev-mode)
;; disable pabbrevs tab completion(?)


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

;; lua support
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)


;; Markdown support
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(autoload 'markdown-mode "markdown-mode" "Markdown editing mode." t)

;; NOTE: this is broken and untabifies all over.
;;(defun real-untabify-hook ()
;;  (prog1 nil
;;    (untabify-buffer)))
;;(defun untabify-hook ()
;;  (add-hook 'before-save-hook 'real-untabify-hook))
;;(add-hook 'markdown-mode-hook 'untabify-hook)


;;(defun cliki:start-slime ()
;;  (unless (slime-connected-p)
;;    (save-excursion (slime))))
;;
;;(add-hook 'slime-mode-hook 'cliki:start-slime)




;; SMOOTH SCROLLING
(require 'smooth-scrolling)




;; TABBAR
(require 'tabbar)

;; Exclude scratch buffers from tabbar
(when (require 'tabbar nil t)
  (setq tabbar-buffer-groups-function
	(lambda (b) (list "All Buffers")))
  (setq tabbar-buffer-list-function
	(lambda ()
	  (remove-if
	   (lambda(buffer)
	     (find (aref (buffer-name buffer) 0) " *"))
	   (buffer-list))))
  (tabbar-mode))





;; CUSTOMIZE SET
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-flag nil)
 '(js2-indent-on-enter-key t)
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-use-images nil)
 '(tabbar-home-button (quote (("|") "|")))
 '(tabbar-scroll-left-button (quote (("|") "|")))
 '(tabbar-scroll-right-button (quote (("|") "|"))))

;;(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "#aaaaaa"))))
;; '(font-lock-variable-name-face ((t nil)))
;; '(tabbar-button-face ((t (:inherit tabbar-default-face :foreground "#777777"))))
;; '(tabbar-default-face ((t (:inherit variable-pitch :background "#404040" :foreground "#888888" :height 1.1))))
;; '(tabbar-selected-face ((t (:inherit tabbar-default-face :background "#555555" :foreground "#eeeeeb"))))
;; '(tabbar-unselected-face ((t (:inherit tabbar-default-face :background "#333333")))))
;;'(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "#aaaaaa"))))
;;'(tabbar-button-face ((t (:inherit tabbar-default-face :foreground "#dddddd"))))
;;'(tabbar-default-face ((t (:inherit variable-pitch :background "#4f4f4f" :foreground "#999999" :height 0.8))))
;;'(tabbar-selected-face ((t (:inherit tabbar-default-face :background "#d87c23" :foreground "#202020"))))
;;'(tabbar-unselected-face ((t (:inherit tabbar-default-face :background "#5f5f5f"))))
;;'(trailing-whitespace ((((class color)) (:background "#883030")))))

;; FUNCTIONS
(defun reindent-buffer ()
  "Reindent the contents of the entire buffer."
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end)))


;; C++ #if 0 grayness
;; less than perfect at the moment.. needs a ctrl+l to update :/
;; ------------ C Mode
(defun my-cpp-highlight ()
  (setq cpp-known-face '(background-color . "#3f4f3f"))
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list
	'((#("0" 0 1
	     (c-in-sws t fontified t))
	   (background-color . "#4f4f4f")
	   nil both nil)
	  (#("1" 0 1
	     (c-in-sws t fontified t))
	   nil
	   (background-color . "#4f4f4f")
	   both nil)))
  (cpp-highlight-buffer t))

(defun my-c-mode-common-hook ()
  (my-cpp-highlight))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-recenter ()
  "Recenter buffer and refresh highlighting."
  (interactive)
  (recenter)
  (cpp-highlight-buffer t))

(defun my-c-initialization-hook ()
  (define-key c-mode-base-map "\C-l" 'my-c-mode-recenter))

(add-hook 'c-initialization-hook 'my-c-initialization-hook)

(defun dot-emacs ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

;; PYTHON !

;;(add-to-list 'load-path "~/.emacs.d/python")
;;(require 'python)

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
(add-to-load-path (expand-file-name "~/.emacs.d/ruby"))
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

(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

(ad-activate 'yank-pop)

;;(bar-cursor-mode t)

(global-set-key "\M-n" 'cyclebuffer-forward)
(global-set-key "\M-p" 'cyclebuffer-backward)

(defun paste-se-encode-uri-component (str)
  (mapconcat
   (lambda (x)
     (if (or (eq x 60) (eq x 62) (eq x 96)
	     (and (>= x 34) (<= x 38)) (and (>= x 123) (<= x 255))
	     (and (>= x 91) (<= x 94)) (and (>= x 0) (<= x 9))
	     (and (>= x 11) (<= x 32)))
	 (format "%%%x" x)
       (char-to-string x)))
   str ""))

(defun paste-se-query-string (pairs)
  (mapconcat
   (lambda (x) (format "%s=%s"
		       (paste-se-encode-uri-component (car x))
		       (paste-se-encode-uri-component (cdr x))))
   pairs "&"))

(defun paste-se-paste-region (beg end)
  (interactive "r")
  (paste-se-paste-string (buffer-substring beg end))
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)


(defun paste-se-paste-string (paste-string)
  (let ((url-request-method "POST")
	(url-request-data (paste-se-query-string
			   (list (cons "paste" paste-string)
				 (cons "lang" "text")
				 (cons "user" "")
				 (cons "desc" ""))))
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(paste-cb (lambda (&rest args)
		    (set buffer (car (cdr args)))
		    (insert (plist-get (car args) :redirect)))))
    (let ((oldbuf (current-buffer)))
      (url-retrieve "http://paste.se/index.py" 'paste-cb (list oldbuf)))))

;;(define-generic-mode 'ragel-mode
;;  '("//") ;; comment-list
;;  '("Machine" "Alphabet" "GetKey" "Include" "Action" "Fsm" "Access" "Write" "Variable"
;;    "any" "ascii" "extend" "alpha" "digit" "alnum" "lower" "upper" "xdigit" "cntrl" "graph"
;;    "print" "punct" "space" "null"
;;    "fc" "fpc" "fcurs" "ftargs" "fentry"
;;    "fhold" "fexec" "fgoto" "fnext" "fcall" "fret" "fbreak" "ftargs" "fentry"
;;    "while" "switch" "case" "for" "if" "else"
;;    ) ;; keyword-list
;;  '(;;("while\\|for\\|if\\|else" 0 font-lock-keyword-face t)
;;    ("\"[^\"]*\"" 0 font-lock-builtin-face t) ;; font-lock-list
;;    ("'[^']*'" 0 font-lock-string-face t) ;; font-lock-list
;;    ("\[.*\]" 0 font-lock-builtin-face t)) ;; font-lock-list
;;  '(".rl\\'") ;; auto-mode-list
;;  nil) ;; function-list

(defvar ragel-mode-font-lock-keywords
  '(("\\('.*'\\)" 1 'font-lock-string-face)
    ("\\(\\\".*\\\"\\)" 1 'font-lock-string-face)))
;;   '(
;;     ("\\(//.*\\)" 1 'font-lock-comment-face)

;;     ))

(define-derived-mode ragel-mode fundamental-mode "Ragel"
  "Major mode to edit ragel files."
  (set (make-local-variable 'font-lock-keywords)
       '(ragel-mode-font-lock-keywords)))
;;   (set (make-local-variable 'comment-start) "//"))

;; JAVASCRIPT MODE
;;(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;;(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;; IMPROVED JAVASCRIPT MODE
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun avi-kill-line-save (&optional arg)
  "Copy to the kill ring from point to the end of the current line.
    With a prefix argument, copy that many lines from point. Negative
    arguments copy lines backward. With zero argument, copies the
    text before point to the beginning of the current line."
  (interactive "p")
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (if arg (forward-visible-line arg)
	      (end-of-visible-line))
	    (point)))))


;;;; SMEX
(require 'smex)
(setq smex-save-file "~/.emacs.d/smex.save")
(setq smex-prompt-string ">>> ")
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;; QUACK
(require 'quack)
(setq quack-fontify-style 'emacs)

;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")
;;(define-key ropemacs-local-keymap [(control c) (control /)] 'rope-code-assist)

(require 'scala-mode-auto)

(dolist (hook '(
		scala-mode-hook
		))
  (add-hook hook 'my-tab-fix)
  (add-hook hook 'my-set-newline-and-indent))

;; Switch fromm *.<impl> to *.<head> and vice versa
(defun switch-cc-to-h ()
  (interactive)
  (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name))
	  (suffix (match-string 2 buffer-file-name)))
      (cond ((string-match suffix "c\\|cc\\|C\\|cpp")
	     (cond ((file-exists-p (concat name ".h"))
		    (find-file (concat name ".h")))
		   ((file-exists-p (concat name ".hh"))
		    (find-file (concat name ".hh")))
		   ((file-exists-p (concat name ".hpp"))
		    (find-file (concat name ".hpp")))))
	    ((string-match suffix "h\\|hh\\|hpp")
	     (cond ((file-exists-p (concat name ".cc"))
		    (find-file (concat name ".cc")))
		   ((file-exists-p (concat name ".C"))
		    (find-file (concat name ".C")))
		   ((file-exists-p (concat name ".cpp"))
		    (find-file (concat name ".cpp")))
		   ((file-exists-p (concat name ".c"))
		    (find-file (concat name ".c")))))))))


;;(require 'eclim)

;; shell-mode
(defun shell ()
  (interactive)
  (ansi-term "/bin/zsh"))

;; google go mode

(require 'go-mode-load)

(dolist (hook '(
		go-mode-hook
		))
  (add-hook hook 'my-tab-fix)
  (add-hook hook 'my-set-newline-and-indent))
