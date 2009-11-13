(defun krig-base-path (subdir)
  (format "%s/%s" "~/.emacs.d" subdir))

(defun krig-setup-load-path (subdirs)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let* ((my-lisp-dir (krig-base-path ""))
	     (default-directory my-lisp-dir))
	(setq load-path (cons my-lisp-dir load-path))
	(normal-top-level-add-to-load-path
	 subdirs))))

(krig-setup-load-path '("scala-mode" "smart-tab" "color-theme" "yasnippet" "ruby"))


;; C SOURCE FOR EMACS
(defun krig-find-emacs-source ()
  "~/.emacs.d/emacs-snapshot-20090909/src")
(when (eq system-type 'windows-nt)
  (setq find-function-C-source-directory "C:/Program/Emacs/src/src"))
(when (not (eq system-type 'windows-nt))
  (setq find-function-C-source-directory (krig-find-emacs-source)))


;; TRAMP
(require 'tramp)
(setq tramp-completion-reread-directory-timeout nil)
(setq tramp-default-method "scpc")
(add-to-list 'tramp-remote-path "~/bin")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))


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
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")


;; FONT IN WINDOWS
(when (eq system-type 'windows-nt)
  (set-default-font
   "-*-Consolas-normal-r-normal-normal-13-*-*-*-c-*-iso8859-1"))


;; COLOR THEMES
(load (krig-base-path "theme"))


;; SET DEFAULT VARIABLES
(load (krig-base-path "vars"))


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


;; SHOW TABS
;;(show-ws-toggle-show-tabs)
;; make tabs a tiny bit darker than the background color
;;(set-face-background 'show-ws-tab "#3a3a3a")


;; LINE NUMBERS
(require 'linum)

;; SMOOTH SCROLLING
(require 'smooth-scrolling)

;; BAR CURSOR
;;(bar-cursor-mode t)


;; SNIPPETS
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")


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



;; TAB EXPANSION FOR C/C++
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


(load (krig-base-path "modes"))


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


(load (krig-base-path "util"))

;; SERVER
(when (not (eq system-type 'windows-nt))
  (server-start))

;; KEY BINDINGS
;; multi-occur marks all occurrances of regexp (WANT)
;; (global-set-key "\C-z" 'multi-occur)
;; pop-to-mark-command.. i don't quite understand, but sounds useful
;; (global-set-key "\M-j" 'pop-to-mark-command)
;; ..don't really see the need for this
;; (global-set-key "\M-q" 'revert-buffer)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-n" 'cyclebuffer-forward)
(global-set-key "\M-p" 'cyclebuffer-backward)

;;;; SMEX
;; Load smex last to have the command cache uptodate
(require 'smex)
(setq smex-save-file "~/.emacs.d/smex.save")
(setq smex-prompt-string ">>> ")
(smex-initialize)
(smex-auto-update) ;; refresh caches after 60 seconds of idle time
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

