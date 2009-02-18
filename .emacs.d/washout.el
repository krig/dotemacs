;;; -*- Mode: Emacs-Lisp; outline-regexp: " \n;;;;+" -*-

(defconst washout-version 0)

;;;###autoload
(defun washout-wash-out-colour (colour &optional degree)
  "Return a colour string specifying a washed-out version of COLOUR."
  (let ((basec (color-values
		(face-attribute 'default :foreground)))
	(col (color-values colour))
	(list nil))
    (unless degree (setq degree 1))
    (while col
      (push (/ (/ (+ (pop col) 
		     (* degree (pop basec)))
		  (1+ degree))
	       256)
	    list))
    (apply 'format "#%02x%02x%02x" (nreverse list))))

(defun washout-wash-out-face (face &optional degree)
  "Make the foreground colour of FACE appear a bit more pale."
  (let ((colour (face-attribute face :foreground)))
    (unless (eq colour 'unspecified)
      (set-face-attribute face nil
 			  :foreground (washout-wash-out-colour colour degree)))))

(defun washout-find-faces (regexp)
  "Return a list of all faces whose names match REGEXP."
  (delq nil
 	(mapcar (lambda (face)
 		  (and (string-match regexp
 				     (symbol-name face))
 		       face))
 		(face-list))))

(defun washout-wash-out-fontlock-faces (&optional degree)
  (mapc (lambda (elt) 
 	  (washout-wash-out-face elt degree))
 	(delq 'font-lock-warning-face 
 	      (washout-find-faces "^font-lock"))))

;;(when (> (length (defined-colors)) 16)
;;  (washout-wash-out-fontlock-faces 2))

;;;###autoload
(defun washout-version ()
  "Display version of washout."
  (interactive)
  (message "Using washout version %s" washout-version))

;;;###autoload
(defun washout-washout ()
  "Wash out colours one step."
  (interactive)
  (washout-wash-out-fontlock-faces 1))

(provide 'washout)
;;; washout.el ends here
