;; My .emacs
;; Revised structure for 2014
;; Like all very customisable software, once in a while
;; all the packages and tweaks and little snippets of code
;; that you've piled up over the years collapse, and the
;; only way to fix the situation is to start over.
;;
;; Should work for 24.3+.


;; system details
(defun krig-macp () (string-match "apple-darwin" system-configuration))
(defun krig-linuxp () (string-match "linux" system-configuration))
(defun krig-winp () (eq system-type 'windows-nt))
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; personal settings
(progn
  (setq hostname (or (getenv "HOSTNAME") system-name))
  (setq user-full-name "Kristoffer Grönlund")
  (setq user-mail-address (if (string-prefix-p "krigpad" hostname)
			      (concat "kgronlund@" "suse" ".com")
			    (concat "krig@" "koru" ".se"))))


;;; basic settings
(progn
  (setq comment-style 'indent)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq inhibit-startup-message t)
  (setq visible-bell t)
  (auto-compression-mode 1)
  (setq warning-minimum-level :error)
  (fset 'yes-or-no-p 'y-or-n-p)
  (blink-cursor-mode -1)
  (show-paren-mode t)
  (line-number-mode t)
  (column-number-mode t)
  (winner-mode 1) ;; window layout: c-c <left> = undo, c-c <right> = redo
  (global-auto-revert-mode t)
  (setq calendar-week-start-day 1)
  (delete-selection-mode t) ; delete selection when adding new text (like "normal" editors)
  (setq mark-even-if-inactive nil)
  (setq frame-title-format "%b %*") ; nicer window names
  (setq initial-scratch-message ";)\n") ; cleaner scratch buffers
  (setq compilation-skip-threshold 2) ; jump directly to errors with M-p/M-n in compilation-mode
  (setq org-startup-indented t)
  (setq-default ispell-program-name "aspell")

  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tooltip-mode -1)
    (set-fringe-style -1)
    (tool-bar-mode -1)))


;; load common lisp
(require 'cl)


;; set path to emacs source code
(cond ((krig-winp)
       (setq find-function-C-source-directory "C:/Program/Emacs/src/src"))
      ((string-match "ultralix" hostname)
       (setq find-function-C-source-directory "~/src/extern/emacs-24.3/src"))
      ((krig-macp)
       (setq find-function-C-source-directory "~/Personal/Sources/emacs/src")))


;; random functions
(load "functions.el")
;; general editor tools and improvements
(load "tools.el")
;; language modes
(load "language-modes.el")


;; basic keybindings
(global-set-key (kbd "M-[") 'beginning-of-defun)
(global-set-key (kbd "M-]") 'end-of-defun)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "C-c C--") 'snakecase-word-or-region)
(global-set-key (kbd "C-k") 'kill-and-join-forward)
(global-set-key [C-tab] (lambda () (interactive) (insert-char 9 1)))
(global-set-key [C-s-up] 'switch-cc-to-h)


;; start the server
(when (not (eq system-type 'windows-nt))
  (server-start))

;; eshell commands
(load "shell-stuff.el")

;; notmuch
(when (string-prefix-p "krigpad" hostname)
  (load "notmuch-config.el"))

;; set the theme
(load "set-theme.el")


;; finally, open ~/.todo
(find-file "~/.todo")
(rename-buffer "*todo*")
