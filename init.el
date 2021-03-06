;;; init -- My .emacs file.
;;; Commentary:
;;; Revised structure for 2014
;;; Like all very customisable software, once in a while
;;; all the packages and tweaks and little snippets of code
;;; that you've piled up over the years collapse, and the
;;; only way to fix the situation is to start over.
;;;
;;; Should work for 24.3+.
;;;
;;; Code:

;; raise GC threshold during init
(setq gc-cons-threshold 80000000)

;; system details
(defun krig-macp ()
  "Is this a mac?"
  (string-match "apple-darwin" system-configuration))
(defun krig-linuxp ()
  "Is this Linux?"
  (string-match "linux" system-configuration))
(defun krig-winp ()
  "Is this Windows?"
  (eq system-type 'windows-nt))

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(when (krig-linuxp)
  (setq source-directory"~/src/extern/emacs"))

(defun split-window-right-ignore (&optional size)
  "TBH I don't remember exactly what this fixes.  SIZE = ?"
  (if (car size) size (list (/ (window-total-width) 2))))

(advice-add 'split-window-right :filter-args
            'split-window-right-ignore)

;; personal settings
(progn
  (defvar *hostname* (or (getenv "HOSTNAME") (system-name)))
  (setq user-full-name "Kristoffer Grönlund")
  (setq user-mail-address (if (or
                               (string-prefix-p "krigpad" *hostname*)
                               (string-prefix-p "charsiu" *hostname*))
			      (concat "kgronlund@" "suse" ".com")
			    (concat "krig@" "koru" ".se"))))

;; melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not (krig-winp))
  (mapc
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
   '(async
     ;;caml
     ;;cider
     ;;clojure-mode
     ;;dash
     evil
     evil-commentary
     evil-surround
     rust-mode
     epl
     findr
     flycheck
     flycheck-rust
     git-commit
     ;;inf-ruby
     ;;inflections
     jump
     magit
     magit-popup
     ;;pkg-info
     ;;queue
     ;;rinari
     ;;ruby-compilation
     ;;sml-mode
     ;;spinner
     ;;tuareg
     with-editor
     nord-theme)))

(when (not (krig-winp))
  (mapc
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
   '(flycheck)))


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
  (setq blink-matching-paren t)
  (setq mark-even-if-inactive nil)
  (setq frame-title-format "%b %*") ; nicer window names
  (setq initial-scratch-message ";)\n") ; cleaner scratch buffers
  (setq compilation-skip-threshold 2) ; jump directly to errors with M-p/M-n in compilation-mode
  (setq org-startup-indented t)
  (setq-default ispell-program-name "aspell")
  (setq vc-follow-symlinks t)

  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tooltip-mode -1)
    (fringe-mode '(0 . 8))
    (tool-bar-mode -1)
    (linum-relative-global-mode)
    (global-hl-line-mode)))


;; load common lisp
(require 'cl-lib)

;; go evil
;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)


;; set path to emacs source code
(cond ((krig-winp)
       (setq find-function-C-source-directory "C:/Program/Emacs/src/src"))
      ((string-match "ultralix" *hostname*)
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
(progn
  (global-set-key (kbd "M-[") 'beginning-of-defun)
  (global-set-key (kbd "M-]") 'end-of-defun)
  (global-set-key (kbd "M-j") 'join-line)
  (global-set-key (kbd "C-c C--") 'snakecase-word-or-region)
  (global-set-key (kbd "C-k") 'kill-and-join-forward)
  (global-set-key [C-tab] (lambda () (interactive) (insert-char 9 1)))
  (global-set-key [C-s-up] 'switch-cc-to-h)

  (when (krig-macp)
    ;;(setq mac-option-modifier 'super )
    ;;(setq mac-command-modifier 'meta )
    (global-set-key [home] 'beginning-of-line)
    (global-set-key [end] 'end-of-line)))


;; start the server
(defun px-raise-frame-and-give-focus ()
  "Utility function to display the focused window."
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(when (not (eq system-type 'windows-nt))
  (server-start)
  (add-hook 'server-switch-hook 'px-raise-frame-and-give-focus))

;; eshell commands
(load "shell-stuff.el")

;; notmuch
(defun kg-using-notmuch-on-host ()
  "Used to only load notmuch on certain machines."
  (or (string-prefix-p "krigpad" *hostname*)
      (string-prefix-p "dumpling" *hostname*)
          (string-prefix-p "charsiu" *hostname*)
          (string-prefix-p "ultralix" *hostname*)))

(when (kg-using-notmuch-on-host)
  (load "notmuch-config.el"))

;; set the theme
(load "set-theme.el")


;; finally, open ~/.todo
(find-file "~/.todo")
(rename-buffer "*todo*")

(defun indent-4-spaces ()
  "Set indentation to 4 spaces."
  (interactive)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(defun indent-2-spaces ()
  "Set indentation to 2 spaces."
  (interactive)
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(defun indent-tabs ()
  "Set indentation to tabs."
  (interactive)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

;; reset gc-cons-threshold
(setq gc-cons-threshold 800000)
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init ends here
