;; some random functions

(defun exit-emacs ()
  (interactive)
  (save-buffers-kill-emacs))

(defun dot-emacs ()
  "Opens the .emacs file for editing."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  "Tabify current buffer"
  (interactive)
  (tabify (point-min) (point-max)))

(defun zap-space-forward () ; adapted from `delete-horizontal-space'
  "*Delete all spaces, tabs and newlines after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

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

(defun un-camelcase-string (s &optional sep start)
    "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is an underscore \"_\".
    If third argument START is non-nil, convert words after that
    index in STRING."
    (let ((case-fold-search nil))
      (while (string-match "[A-Z]" s (or start 1))
        (setq s (replace-match (concat (or sep "_")
                                       (downcase (match-string 0 s)))
                               t nil s)))
      (downcase s)))

(defun snakecase-region (start end)
  "Changes region from camelCase to snake_case"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (let ((s (un-camelcase-string (buffer-substring (point-min) (point-max)))))
                      (delete-region (point-min) (point-max))
                      (goto-char (point-min))
                      (insert s))))

(defun snakecase-word-or-region ()
  "Changes word or region from camelCase to snake_case"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (snakecase-region pos1 pos2)))

(defun mrc-dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun now ()
  "Insert string for the current time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(defun open-worklog ()
  "creates the worklog for today if it doesn't exist and opens it in emacs"
  (interactive)
  (let ((fn (shell-command-to-string "sh ~/annex/doc/work/SUSE/create-entry.sh")))
    (find-file (s-trim fn))))

(defun reindent-buffer ()
  "Reindent the contents of the entire buffer."
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end)))

(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
    Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

(defun create-scratch-buffer ()
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (format "*scratch%s*" (if (= n 0) "" n))
                   n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

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