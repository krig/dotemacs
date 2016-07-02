;; NOTMUCH

(defun krig-notmuch-mark-as-read ()
  "notmuch: mark as read toggle"
  (interactive)
  (notmuch-search-tag
   (if (member "unread" (notmuch-search-get-tags))
       "-unread" "+unread")))

(defun my-notmuch-show-view-as-patch ()
  "View the the current message as a patch."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (notmuch-get-bodypart-internal id 1 nil)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
                 (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

(add-to-list 'load-path "~/.emacs.d/tools/notmuch/emacs")
(when (require 'notmuch nil 'noerror)
  (setq mail-user-agent 'message-user-agent)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-on-exit t)
  ;(setq notmuch-search-oldest-first nil) ;; second thought on this :P
  (define-key 'notmuch-show-mode-map "D" 'my-notmuch-show-view-as-patch)
  ;;(require 'notmuch-address)
  ;;(setq notmuch-address-command (expand-file-name "~/bin/nottoomuch-addresses.sh"))
  (setq notmuch-crypto-process-mime t))
  ;;(notmuch-address-message-insinuate))
