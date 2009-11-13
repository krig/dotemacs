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

;; FONT IN WINDOWS
(when (eq system-type 'windows-nt)
  (set-default-font
   "-*-Consolas-normal-r-normal-normal-13-*-*-*-c-*-iso8859-1"))

;; COLOR THEMES
(load (krig-base-path "theme"))

;; UNTABIFY
(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

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

;; SET DEFAULT VARIABLES
(load (krig-base-path "vars"))


;; TRAMP
(require 'tramp)
(setq tramp-completion-reread-directory-timeout nil)
(setq tramp-default-method "scpc")
(add-to-list 'tramp-remote-path "~/bin")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))


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


;; SHOW TABS
;;(show-ws-toggle-show-tabs)
;; make tabs a tiny bit darker than the background color
;;(set-face-background 'show-ws-tab "#3a3a3a")


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



;; LINE NUMBERS
(require 'linum)

;; SNIPPETS
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

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

;; FUNCTIONS
(defun reindent-buffer ()
  "Reindent the contents of the entire buffer."
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end)))

(defun dot-emacs ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

;; YANK POP
(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))
(ad-activate 'yank-pop)


;;(bar-cursor-mode t)

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

;; SHELL w/ COLOR SUPPORT
(defun shell ()
  (interactive)
  (ansi-term "/bin/zsh"))


