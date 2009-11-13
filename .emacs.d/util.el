;;;; UTILITY FUNCTIONS

;; HEADER SWITCH
(defun switch-cc-to-h ()
  "Switch fromm *.<impl> to *.<head> and vice versa."
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
  "Opens a shell prompt with ANSI color support."
  (interactive)
  (ansi-term "/bin/zsh"))

;; KILL-RING-SAVE
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

;; CREATE SCRATCH BUFFER
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


(defun reindent-buffer ()
  "Reindent the contents of the entire buffer."
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end)))


(defun dot-emacs ()
  "Opens the .emacs file for editing."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))


(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))
(ad-activate 'yank-pop)
