(c-add-style "xmms2"
             '("K&R"
               (c-basic-offset . 4)
               (c-offsets-alist
                (case-label . +)
                (arglist-close . c-lineup-arglist-intro-after-paren)
                (inextern-lang . 0))))

(defun maybe-xmms2-style ()
  "Set the style used in xmms2, requires smart tabs."
  (when (string-match "xmms2" buffer-file-name)
        (setq show-trailing-whitespace t)
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (c-set-style "xmms2")))

(add-hook 'c-mode-common-hook 'maybe-xmms2-style)

(provide 'xmms2-style)
