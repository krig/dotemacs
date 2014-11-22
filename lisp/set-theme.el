;; loads and sets the theme

;; set font
(when (display-graphic-p)
  (when (krig-macp)
    (setq mac-allow-anti-aliasing t)
    (if (> (display-pixel-width) 1900)
        (set-frame-font "Inconsolata-15")
      (set-frame-font "Inconsolata-13"))
                                        ; nicer lambdas
    (set-fontset-font "fontset-default"
                      'greek-iso8859-7
                      '("Consolas" . "iso10646-1")))

  (when (krig-linuxp)
    (set-frame-font
     (pcase hostname
       ("walker.kri.gs" "Ubuntu Mono-19.5")
       ("kowloon" "Ubuntu Mono-17")
       ("krigpad.site" "Ubuntu Mono-14")
       (_ "Ubuntu Mono-13")))
    (menu-bar-mode -1))

  ;; use the after-init-hook to work around a scrollbar bug..
  (add-hook 'after-init-hook
            '(lambda ()
               (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tomorrow")
               (load-theme 'tomorrow-night-eighties)
               (scroll-bar-mode -1))))

(unless (display-graphic-p)
  (menu-bar-mode -1))
