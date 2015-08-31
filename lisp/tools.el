;; various tools and non-language modes I use

;; ido
(progn
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-save-directory-list-file "~/.emacs.d/ido.history")
  (setq ido-max-work-file-list 40)
  (setq ido-enable-flex-matching t)
  (setq confirm-nonexistent-file-or-buffer nil)
  (add-hook 'ido-define-mode-map-hook 'ido-my-keys)

  (defun ido-my-keys ()
    (define-key ido-mode-map "\t" 'ido-complete)
    (define-key ido-mode-map "\C-t" 'ido-toggle-regexp) ; same as in isearch
    (define-key ido-mode-map "\C-d" 'ido-enter-dired)) ; cool
  (global-set-key (kbd "C-x C-c") 'ido-switch-buffer)

  (defun aw-ido-completing-read-with-default (prompt entries predicate)
    (let* ((maybedft (find-tag-default))
	   (compl (all-completions "" entries predicate))
	   (dft (assoc-string maybedft compl)))
      (ido-completing-read prompt compl nil t nil nil dft))))

;; uniquify
(when (require 'uniquify nil 'noerror)
  (setq uniquify-buffer-name-style 'post-forward
	uniquify-separator ":"))

;; recentf
(when (require 'recentf nil 'noerror)
  (recentf-mode t)
  (setq recentf-max-menu-items 40)

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
  (global-set-key "\C-x\C-r" 'recentf-ido-find-file))


;; idomenu
(setq imenu-auto-rescan t)
(autoload 'idomenu "idomenu" nil t)
(global-set-key "\C-x\C-\\" 'idomenu)


;; sr-speedbar
(when (load "sr-speedbar.el" 'noerror)
  (setq speedbar-use-images nil)
  (make-face 'speedbar-face)
  (when (display-graphic-p)
    (set-face-font 'speedbar-face "Ubuntu Mono-11"))
  (setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))
  (sr-speedbar-refresh-turn-on)
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-skip-other-window-p nil)
  (mapcar 'speedbar-add-supported-extension '(".hs" ".rb"))
  (global-set-key (kbd "M-p") 'sr-speedbar-toggle)
  (global-set-key (kbd "C-x p") 'sr-speedbar-select-window))

;; unbound
(require 'unbound nil 'noerror)

;; smex
(progn
  (add-to-list 'load-path "~/.emacs.d/tools/smex")
  (require 'smex)
  (setq smex-save-file "~/.emacs.d/smex.save")
  (setq smex-prompt-string "M-x ")
  (smex-initialize)
  (smex-auto-update) ;; refresh caches after 60 seconds of idle time
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(put 'ido-exit-minibuffer 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ignoramus - ignore uninteresting files
(require 'ignoramus)
(ignoramus-setup)

;; whitespace
(require 'whitespace)
(setq whitespace-line-column 130)


;; smart-tab
(progn
  (add-to-list 'load-path "~/.emacs.d/tools/smart-tab")
  (require 'smart-tab)
  (with-eval-after-load 'smart-tab
    (global-smart-tab-mode 1)
    (add-to-list 'smart-tab-disabled-major-modes 'message-mode)
    (setq smart-tab-using-hippie-expand t)
    (require 'hippie-exp)
    (setq hippie-expand-try-functions-list
          '(try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))))

;; smarttabs
(progn
  (add-to-list 'load-path "~/.emacs.d/tools/smarttabs")
  (autoload 'smart-tabs-mode "smart-tabs-mode"
    "Intelligently indent with tabs, align with spaces!")
  (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
  (autoload 'smart-tabs-advice "smart-tabs-mode")
  (autoload 'smart-tabs-insinuate "smart-tabs-mode"))

;; browse-kill-ring+
(progn
  (require 'browse-kill-ring+)
  (browse-kill-ring-default-keybindings)

  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    (if (not (eq last-command 'yank))
	(browse-kill-ring)
      ad-do-it))
  (ad-activate 'yank-pop))

;; fixme: in comments
(require 'fic-mode)


;; load and set up tramp properly
(progn
  (require 'tramp)
  (setq tramp-completion-reread-directory-timeout nil)
  (setq tramp-default-method "scp")
  ;;(add-to-list 'tramp-remote-path "~/bin")
  (setq vc-ignore-dir-regexp
	(format "\\(%s\\)\\|\\(%s\\)"
		vc-ignore-dir-regexp
		tramp-file-name-regexp)))

;; eshell
(progn
  (require 'eshell)
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t))

;; search for symbol at point
;; http://blog.jorgenschaefer.de/2012/11/emacs-search-for-symbol-at-point.html
(progn
  (define-key isearch-mode-map (kbd "C-d")
    'fc/isearch-yank-symbol)
  (defun fc/isearch-yank-symbol ()
    "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch, but it only looks for
the rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
    (interactive)
    (isearch-yank-string
     (save-excursion
       (when (and (not isearch-forward)
		  isearch-other-end)
	 (goto-char isearch-other-end))
       (thing-at-point 'symbol)))))


;; open for dired
(require 'dired-x)
(with-eval-after-load 'dired-x
  (defun dired-open-mac ()
    (interactive)
    (let ((file-name (dired-get-file-for-visit)))
      (if (file-exists-p file-name)
	  (call-process "/usr/bin/open" nil 0 nil file-name))))

  (when (krig-macp)
    (define-key dired-mode-map "o" 'dired-open-mac)
    (setq ls-lisp-use-insert-directory-program t)      ;; use external ls
    (setq insert-directory-program "/usr/local/bin/gls")))


;; git-messenger
(add-to-list 'load-path "~/.emacs.d/tools/popup")
(add-to-list 'load-path "~/.emacs.d/tools/git-messenger")
(require 'git-messenger)
(with-eval-after-load 'git-messenger
  (setq git-messenger:show-detail t)
  (define-key git-messenger-map (kbd "C-k") 'git-messenger:copy-message)
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message))

;; textmate
;; for M-t => go to file in project
(add-to-list 'load-path "~/.emacs.d/tools/textmate")
(require 'textmate)
(textmate-mode)


;; magit
;; http://whattheemacsd.com/setup-magit.el-01.html
;; full screen magit-status


(with-eval-after-load 'magit
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  ;; workaround issue magit/magit#2220
  (defun magit-toplevel (&optional file strict)
    (magit--with-safe-default-directory file
      (-if-let (cdup (magit-rev-parse-safe "--show-cdup"))
          (magit-expand-git-file-name
           (file-name-as-directory (expand-file-name cdup)))
        (unless strict
          (-when-let (gitdir (magit-git-dir))
            (if (magit-bare-repo-p)
                gitdir
            (file-name-directory (directory-file-name gitdir))))))))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(global-set-key (kbd "C-c s") 'magit-status)

(require 'rfringe)

;; diminish
(require 'diminish)

;; paredit
(progn
  (add-to-list 'load-path "~/.emacs.d/tools/paredit")
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)

  (add-hook 'clojure-mode-hook (lambda () (enable-paredit-mode)))
  (add-hook 'cider-repl-mode-hook (lambda () (enable-paredit-mode)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit-mode)))

  (with-eval-after-load 'paredit
    ;; don't hijack \
    (define-key paredit-mode-map (kbd "\\") nil)

    ;; making paredit work with delete-selection-mode
    (put 'paredit-forward-delete 'delete-selection 'supersede)
    (put 'paredit-backward-delete 'delete-selection 'supersede)
    (put 'paredit-newline 'delete-selection t)))

