;; emacs configuration
(autoload 'magit-status "magit" nil t)

(push "/usr/local/bin" exec-path)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/smart-tab")

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(setq visible-bell t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(winner-mode 1)
(global-auto-revert-mode t)

;; put something different in the scratch buffer
(setq initial-scratch-message
      ";; scratch buffer\n")

(setq compilation-skip-threshold 2)

(setq-default ispell-program-name "aspell")

;; TRAMP
(require 'tramp)
(setq tramp-completion-reread-directory-timeout nil)
(setq tramp-default-method "scpc")
;;(add-to-list 'tramp-remote-path "~/bin")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))


;; ELPA
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(require 'el-get)

(setq el-get-sources
      '((:name ruby-mode
               :type elpa
               :load "ruby-mode.el"
               :after (lambda () (ruby-mode-hook)))
        (:name inf-ruby  :type elpa)
        (:name ruby-compilation :type elpa)
        (:name css-mode
               :type elpa
               :load "css-mode.el"
               :after (lambda () (css-mode-hook)))
        (:name sass-mode
               :type elpa
               :load "sass-mode.el"
               :after (lambda () (sass-mode-hook)))
        (:name textmate
               :type git
               :url "git://github.com/defunkt/textmate.el"
               :load "textmate.el")
;;        (:name rvm
;;               :type git
;;               :url "http://github.com/djwhitt/rvm.el.git"
;;               :load "rvm.el"
;;               :compile ("rvm.el")
;;               :after (lambda() (rvm-use-default)))
        (:name rhtml
               :type git
               :url "https://github.com/eschulte/rhtml.git"
               :features rhtml-mode
               :after (lambda () (rhtml-mode-hook)))
        (:name yaml-mode
               :type git
               :url "http://github.com/yoshiki/yaml-mode.git"
               :features yaml-mode
               :after (lambda () (yaml-mode-hook)))
        (:name magit
               :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))))

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))
(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-hook ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

(defun sass-mode-hook ()
  (autoload 'sass-mode "css-mode" nil t))

(defun is-rails-project ()
  (when (textmate-project-root)
    (file-exists-p (expand-file-name "config/environment.rb" (textmate-project-root)))))

(defun run-rails-test-or-ruby-buffer ()
  (interactive)
  (if (is-rails-project)
      (let* ((path (buffer-file-name))
             (filename (file-name-nondirectory path))
             (test-path (expand-file-name "test" (textmate-project-root)))
             (command (list ruby-compilation-executable "-I" test-path path)))
        (pop-to-buffer (ruby-compilation-do filename command)))
    (ruby-compilation-this-buffer)))

;; SYNC EL-GET
;;(el-get 'sync)
(el-get)

;; TEXTMATE
(textmate-mode)

;; PARENFACE
(require 'parenface)

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
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)

;; SMART TAB
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))




;; UNIQUIFY - better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; RECENT FILES
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)

;; OPEN FOR DIRED
(require 'dired-x)
(defun dired-open-mac ()
       (interactive)
       (let ((file-name (dired-get-file-for-visit)))
         (if (file-exists-p file-name)
             (call-process "/usr/bin/open" nil 0 nil file-name))))
(define-key dired-mode-map "o" 'dired-open-mac)


;; PASTE.SE
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

;; UNTABIFY
(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(require 'generic-x)

;; CREATE SCRATCH BUFFER

(define-generic-mode 'scratch-mode
  '("#" ";") ; comment-list
  '() ; keyword-list
  '(("[0-9]+" . 'font-lock-variable-name-face)) ; font-lock-list
  nil ; auto-mode-list
  '(auto-fill-mode) ; function-list
  "Major mode for scratch buffers.")

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
    (scratch-mode)))

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

;; BDSM

(defvar bdsm-tab-width 4)

(defun bdsm-mode-setup () ; use c-indent-command
  (setq tab-width bdsm-tab-width)
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (smart-tab-mode))

(define-generic-mode 'bdsm-mode
  '("#")
  '("any" "all" "in" "not")
  '(("[a-z][a-zA-Z0-9_]*" . 'font-lock-reference-face)
    ("[0-9]+" . 'font-lock-variable-name-face)
    ("^[ ]+" . 'trailing-whitespace)
    ("/" . 'font-lock-type-face)
    ("?" . 'font-lock-builtin-face)
    ("!" . 'font-lock-function-name-face))
  '()
  '(bdsm-mode-setup)
  "A mode for BDSM grammars")

;; HEADER SWITCH
(defun switch-cc-to-h ()
  "Switch fromm *.<impl> to *.<head> and vice versa."
  (interactive)
  (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name))
	  (suffix (match-string 2 buffer-file-name)))
      (cond ((string-match suffix "c\\|cc\\|C\\|cpp\\|m")
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
		    (find-file (concat name ".c")))
       ((file-exists-p (concat name ".m")))))))))
(global-set-key [C-s-up] 'switch-cc-to-h)

;; REINDENT BUFFER
(defun reindent-buffer ()
  "Reindent the contents of the entire buffer."
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end)))

;; OPEN INIT.EL
(defun dot-emacs ()
  "Opens the .emacs file for editing."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

;; BROWSE KILL RING
(require 'browse-kill-ring+)
(browse-kill-ring-default-keybindings)

(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))
(ad-activate 'yank-pop)

;; SERVER
(when (not (eq system-type 'windows-nt))
  (server-start))

(load "~/.emacs.d/nxhtml/autostart.el")

(defun krig-sh-mode-hook ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (local-set-key [return] 'newline-and-indent)
  (whitespace-mode))

(defun krig-mode-hook ()
  (setq show-trailing-whitespace t)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent))

(defun my-c-style-fix ()
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(defun my-java-style-fix ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

;; newline-and-indent
(defun my-set-newline-and-indent()
  (local-set-key [return] 'newline-and-indent))

(dolist (hook '(
		c-mode-hook
		c++-mode-hook
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
  (add-hook hook 'krig-mode-hook)
  (add-hook hook 'my-set-newline-and-indent))

(add-hook 'sh-mode-hook 'krig-sh-mode-hook)

(add-hook 'c-mode-hook 'my-c-style-fix)
(add-hook 'c++-mode-hook 'my-c-style-fix)
(add-hook 'objc-mode-hook 'my-c-style-fix)
(add-hook 'java-mode-hook 'my-java-style-fix)

(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))


;; '(js2-basic-offset 4)
;; '(js2-bounce-indent-flag nil)
;; '(js2-indent-on-enter-key t)

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
(defun mypy-extra-stuff ()
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil))
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

;; GOOGLE GO MODE
(require 'go-mode-load)

(defun my-go-style-fix ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(dolist (hook '(
		go-mode-hook
		))
  (add-hook hook 'my-go-style-fix)
  (add-hook hook 'krig-mode-hook))

;; IMPROVED JAVASCRIPT MODE
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(load "js2-setup.el")

(defun krig-js2-style-fix ()
  (setq show-trailing-whitespace t))
(add-hook 'js2-mode-hook 'krig-js2-style-fix)

;; MUSTACHE MODE
;;(add-to-list 'load-path "~/.emacs.d/mustache-mode.el")
(require 'mustache-mode)


;; RUBY HTML.ERB MODE
(setq
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 5
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxml-auto-validate-flag nil
 nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; HAML-MODE
(add-to-list 'load-path "~/.emacs.d/haml-mode")
(require 'haml-mode)


;; SLIME

(add-to-list 'load-path "~/.emacs.d/slime/")  ; your SLIME directory
(setq inferior-lisp-program "sbcl --noinform") ; your Lisp system
(require 'slime)
(slime-setup '(slime-fancy))

;; QUACK

(require 'quack)

(autoload 'bison-mode "bison-mode.el")
(add-to-list 'auto-mode-alist '("\\.ypp\\'" . bison-mode))

(autoload 'flex-mode "flex-mode.el")
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))

;; PARROT

(load "parrot")
(load "pasm")
(add-to-list 'auto-mode-alist '("\\.pasm\\'" . pasm-mode))
(add-hook 'pasm-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil))))
(autoload 'pir-mode "pir-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pir\\'" . pir-mode))

;; MAGIT
(require 'magit)

;; IDO RECENTF
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
		    (cons (file-name-nondirectory x)
			  x))
		  recentf-list))
	 (filename-list
	  (remove-duplicates (mapcar #'car file-assoc-list)
			     :test #'string=))
	 (filename (ido-completing-read "Choose recent file: "
					filename-list
					nil
					t)))
    (when filename
      (find-file (cdr (assoc filename
			     file-assoc-list))))))
(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

(defun zap-space-forward () ; adapted from `delete-horizontal-space'
  "*Delete all spaces, tabs and newlines after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

(global-set-key [C-tab] (lambda () (interactive) (insert-char 9 1)))

;;(require 'undo-tree)
;;(global-undo-tree-mode)


;; RT :(

(add-to-list 'load-path "~/.emacs.d/rt-liberation/")
(require 'rt-liberation)
(setq rt-liber-rt-binary "~/bin/rt"
      rt-liber-rt-version "3.8.2")
(setq rt-liber-username "krig")
(setq rt-liber-base-url "https://tracker.int.prnw.net/")

;; COFFEESCRIPT
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
;; coffee uses 2 spaces
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; SMEX
(progn
  (add-to-list 'load-path "~/.emacs.d/smex")
  (require 'smex)
  (setq smex-save-file "~/.emacs.d/smex.save")
  (setq smex-prompt-string "M-x ")
  (smex-initialize)
  (smex-auto-update) ;; refresh caches after 60 seconds of idle time
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-mode t)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("b0950b032aa3c8faab4864ae288296dd66b92eca" "0174d99a8f1fdc506fa54403317072982656f127" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "36c5ca198b60e4ac862195b3f0533ad31cc1a4ff" "f9d68c6c4216f3afd89d4439fd378a5dce869034" "e17065576593ed80494c2e275e151805bb9428a8" default)))
 '(quack-default-program "racket")
 '(quack-remap-find-file-bindings-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; THEME
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized-dark)

(set-frame-font "Ubuntu Mono-14")

(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t))

(defun procera-psm-tickets ()
  "List relevant PSM tickets in RT"
  (interactive)
  (rt-liber-browse-query "( Status = 'new' OR Status = 'open' OR Status = 'stalled' ) AND (  'CF.{Component}' LIKE 'PSM' OR Queue = 'PSMBugs' OR Queue = 'PSMFeatures' )"))

;; (defun procera-psm-open-ticket ()
;;   "Open specific ticket in RT"
;;   (interactive)
;;   (let ((ticket (read-string "Ticket ID:")))
;;     (rt-liber-browse-query
;;      (rt-liber-compile-query
;;       (id ticket)))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
  (message "Renamed to %s." new-name)))

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond
  ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
  (t
   (let* ((w1 (first (window-list)))
          (w2 (second (window-list)))
          (b1 (window-buffer w1))
          (b2 (window-buffer w2))
          (s1 (window-start w1))
          (s2 (window-start w2)))
     (set-window-buffer w1 b2)
     (set-window-buffer w2 b1)
     (set-window-start w1 s2)
     (set-window-start w2 s1)))))

;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun set-tab-stop-width (width)
      "Set all tab stops to WIDTH in current buffer.
    This updates `tab-stop-list', but not `tab-width'.
    By default, `indent-for-tab-command' uses tabs to indent, see
    `indent-tabs-mode'."
      (interactive "nTab width: ")
      (let* ((max-col (car (last tab-stop-list)))
             ;; If width is not a factor of max-col,
             ;; then max-col could be reduced with each call.
             (n-tab-stops (/ max-col width)))
        (set (make-local-variable 'tab-stop-list)
             (mapcar (lambda (x) (* width x))
                     (number-sequence 1 n-tab-stops)))
        ;; So preserve max-col, by adding to end.
        (unless (zerop (% max-col width))
          (setcdr (last tab-stop-list)
                  (list max-col)))))

(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
        (call-interactively 'backward-delete-char-untabify))))))

