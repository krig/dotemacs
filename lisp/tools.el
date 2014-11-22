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
    (set-face-font 'speedbar-face "Ubuntu Mono-9"))
  (setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))
  (sr-speedbar-refresh-turn-on)
  (speedbar-add-supported-extension ".hs")
  (global-set-key (kbd "M-p") 'sr-speedbar-toggle))

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

;; whitespace - 
(require 'whitespace)
(setq whitespace-line-column 130)


;; smart-tab
(progn
  (add-to-list 'load-path "~/.emacs.d/tools/smart-tab")
  (when (require 'smart-tab nil 'noerror)
    (global-smart-tab-mode 1)
    (add-to-list 'smart-tab-disabled-major-modes 'message-mode)
    (setq smart-tab-using-hippie-expand t)
    (require 'hippie-exp)
    (setq hippie-expand-try-functions-list
	  '(try-expand-dabbrev
	    try-expand-dabbrev-all-buffers
	    try-expand-dabbrev-from-kill
	    try-complete-lisp-symbol-partially
	    try-complete-lisp-symbol
	    try-complete-file-name-partially
	    try-complete-file-name))))

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
(progn
  (require 'dired-x)
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
(when (require 'git-messenger nil 'noerror)
  (setq git-messenger:show-detail t)
  (define-key git-messenger-map (kbd "C-k") 'git-messenger:copy-message)
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message))

;; textmate
;; for M-t => go to file in project
(add-to-list 'load-path "~/.emacs.d/tools/textmate")
(require 'textmate)
(textmate-mode)


;; rainbow-delimiters
(when (display-graphic-p)
  (add-to-list 'load-path "~/.emacs.d/tools/rainbow-delimiters")
  (progn
    (defun krig-paren-clr (n)
      (let ((c (+ ?\x69 (* (1- n) 8))))
        (format "#%X%X%X" c c c)))

    (defun krig-rainbow-face-n (n)
      (intern (format "rainbow-delimiters-depth-%d-face" n)))

    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
    (cl-loop for i from 1 to 9 do
             (set-face-foreground (krig-rainbow-face-n i)
                                  (krig-paren-clr i)))))
