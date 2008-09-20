;;;
;; sr-speedbar.el
;;
;; Copyright (c) 2008 Sebastian Rose, Hannover, Germany
;;               sebastian_rose at gmx de
;;
;; Released under the GNU General Public License v. 2
;;
;; The idea and the starting point for this code was found here:
;;             http://www.emacswiki.org/cgi-bin/wiki/SpeedBar
;;
;; The author of the original code snippet is unknown.
;;
;; Usage:
;; 1.) in .emacs (or where ever your startup lives in):
;;
;;      (require 'sr-speedbar)
;;      (global-set-key [(super ?s)] 'sr-speedbar-toggle)
;;
;; ...or any key binding you like.
;; Now your windows key and 's' show the speedbar in an extra window, same
;; frame. Currently speedbar always shows up on the right. You can customize
;; the initial width of the speedbar window further down for console/DOS and
;; X/Win/MacOS seperatly.
;;
;;
;; Added some lines to get it working:
;;     * splitting the window and remember it,
;;     * changing the way speedbar finds a file.
;;     * File view of speedbar is now working all right.
;;     * C-x 1 in other window deletes speedbar-window, just calling
;;       M-x sr-speedbar-no-separate-frame again is fine now.
;;     * Toggle speedbar works, width is save when toggeling.
;;     * Recalc speedbar width if window-width - speedbar-width <= 0
;;     * Speedbar window is now dedicated to speedbar-buffer.
;;;

;;;
;; TODO:
;;
;; * Update contents of Speedbar, when loading file(s) from an other directory or
;;   loading mew (as an example).
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Customization
;;
(defvar sr-speedbar-width-x 24 "Initial width of sr-speedbar-window under window system.")
(defvar sr-speedbar-width-console 24 "Initial width of sr-speedbar-window on console.")
;;
;;                         END OF CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'speedbar)

(defconst sr-speedbar-version "0.0.2")

(defvar sr-speedbar-width 24 "intial width of speedbar-window")
(defun sr-speedbar-recalculate ()
  "Calculate the speedbar width with respect of window system"
  (if (and window-system (not (string= "pc" window-system)))
      (setq sr-speedbar-width sr-speedbar-width-x)
    (setq sr-speedbar-width sr-speedbar-width-console)))

(sr-speedbar-recalculate)

(defconst sr-speedbar-buffer-name "*SPEEDBAR*")
(defvar sr-speedbar-window nil "speedbar window")
(defvar speedbar-buffer nil "speedbar buffer")


(defun sr-sp-before-visiting-file-hook () "" (select-window (previous-window)))
(defun sr-sp-before-visiting-tag-hook () "" (select-window (previous-window)))
(defun sr-sp-visiting-file-hook () "" (select-window (previous-window)))
(defun sr-sp-visiting-tag-hook () "" (select-window (previous-window)))


(defun sr-speedbar-no-separate-frame ()
  (interactive)
  (let ( (win (selected-window)) )
    (if (not (buffer-live-p speedbar-buffer))
        (progn
          (delete-other-windows) ;; ensure only one window is there
          (if (<= (window-width) sr-speedbar-width)
              (sr-speedbar-recalculate))
          (setq sr-speedbar-window (split-window (selected-window) (- (window-width) sr-speedbar-width) t))
          (setq speedbar-buffer (get-buffer-create sr-speedbar-buffer-name)
                speedbar-frame (selected-frame)
                dframe-attached-frame (selected-frame)
                speedbar-select-frame-method 'attached
                speedbar-verbosity-level 0
                speedbar-last-selected-file nil)
          (set-buffer speedbar-buffer)
          (speedbar-mode)
          (speedbar-reconfigure-keymaps)
          (speedbar-update-contents)
          (speedbar-set-timer 1)
          (add-hook 'speedbar-before-visiting-file-hook 'sr-sp-before-visiting-file-hook t)
          (add-hook 'speedbar-before-visiting-tag-hook 'sr-sp-before-visiting-tag-hook t)
          (add-hook 'speedbar-visiting-file-hook 'sr-sp-visiting-file-hook t)
          (add-hook 'speedbar-visiting-tag-hook 'sr-sp-visiting-tag-hook t)
          ; (make-local-hook 'kill-buffer-hook) ; depricated. uncomment for emacs 21
          (add-hook 'kill-buffer-hook
                    (lambda () (when (eq (current-buffer) speedbar-buffer)
                                 (progn
                                   (setq speedbar-frame nil
                                         dframe-attached-frame nil
                                         speedbar-buffer nil)
                                   (remove-hook 'speedbar-before-visiting-file-hook 'sr-sp-before-visiting-file-hook)
                                   (remove-hook 'speedbar-before-visiting-tag-hook 'sr-sp-before-visiting-tag-hook)
                                   (remove-hook 'speedbar-visiting-file-hook 'sr-sp-visiting-file-hook)
                                   (remove-hook 'speedbar-visiting-tag-hook 'sr-sp-visiting-tag-hook))
                                 (speedbar-set-timer nil)))))
      (if (not (window-live-p sr-speedbar-window))
          (progn
            (delete-other-windows) ;; ensure only one window is there
            (setq sr-speedbar-window
                  (split-window (selected-window)
                                (- (window-width) sr-speedbar-width) t)))))
    (set-window-buffer sr-speedbar-window (get-buffer sr-speedbar-buffer-name))
    (set-window-dedicated-p sr-speedbar-window t)
    (select-window win)
    (bury-buffer speedbar-buffer)
    ))



(defun sr-speedbar-toggle ()
  "Toggle visibility of sr-speedbar by resizing the sr-speedbar-window to a minimal width
or the last width when visible. Use this function to create or toggle visibility
of a speedbar-window. It will be created if neccessary."
  (interactive)
  (if (or (not (window-live-p sr-speedbar-window)) (not (buffer-live-p speedbar-buffer)))
      (progn
        (sr-speedbar-no-separate-frame)
        (setq sr-speedbar-width (window-width sr-speedbar-window)))
    (let ( (win (selected-window)) )
      (let ( (w (window-width sr-speedbar-window)) )
      (if (<= w 1)
          (progn
            (select-window sr-speedbar-window)
            (enlarge-window-horizontally (- sr-speedbar-width 1)))
        (select-window sr-speedbar-window)
        (setq sr-speedbar-width (window-width))
        (shrink-window-horizontally (- sr-speedbar-width 1))))
      (bury-buffer speedbar-buffer)
      (if (window-live-p win)
          (select-window win)))))


(provide 'sr-speedbar)
