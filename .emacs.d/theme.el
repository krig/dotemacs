;;; COLOR THEME
;;(load-library "zenburn")
;;(color-theme-zenburn)

(require 'color-theme)
(load-file "~/.emacs.d/themes/color-theme-twilight.el")
(color-theme-twilight)

;;(require 'color-theme)
;;(load-file "~/.emacs.d/themes/color-theme-krig.el")
;;(color-theme-krig)

;;(load-file "~/.emacs.d/themes/color-theme-inkpot.el")
;;(load-library "color-theme-inkpot")
;;(color-theme-inkpot)

;;(color-theme-initialize)
;;(load-file "~/.emacs.d/themes/color-theme-blackboard.el")
;;(color-theme-blackboard)
;;(color-theme-black-on-gray)

(set-face-font 'tabbar-default-face "silkscreen-6")

(defun my-font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))
