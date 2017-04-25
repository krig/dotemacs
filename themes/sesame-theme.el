;;; sesame-theme.el --- sesame theme

;; Copyright (C) 2017 Kristoffer Gronlund <krig@koru.se>

;; Author: Kristoffer Gronlund <krig@koru.se>
;; URL: https://github.com/krig/sesame-theme
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Black on white.

;;; Code:

(deftheme sesame
  "Black on white.")

(let* ((dark "#222222")
       (light "#fcfcfc")
       (match-bg "#f1c40f")
       (shade-bg "#eeeeec")
       (faces `((default ((t (:background ,light :foreground ,dark))))
                (mouse ((t (:foreground ,dark))))
                (cursor ((t (:background ,dark))))
                (border ((t (:foreground ,dark))))
                (show-paren-match-face ((t (:foreground ,dark :background ,match-bg))))
                (match ((t (:foreground ,light :background ,match-bg))))
                (isearch ((t (:foreground ,light :background ,dark))))
                (isearch-lazy-highlight-face ((t (:foreground ,dark :background ,match-bg))))
                (notmuch-wash-cited-text ((t (:foreground ,dark :background ,shade-bg))))
                (widget-field ((t (:foreground ,dark :background ,shade-bg))))
                )))
  (dolist (face (face-list))
    (cond ((memq face '(bold bold-italic))
           (add-to-list 'faces `(,face ((t (:bold t))))))
          ((memq face '(italic underline show-paren-mismatch-face))
           (add-to-list 'faces `(,face ((t (:underline t))))))
          ((memq face '(modeline modeline-buffer-id modeline-mousable
                                 modeline-mousable-minor-mode highlight region
                                 secondary-selection show-paren-match-face))
           (add-to-list 'faces `(,face ((t (:foreground ,light
                                            :background ,dark
                                            :inverse t))))))
          (t
           (add-to-list 'faces `(,face ((t (nil))))))))
  (apply 'custom-theme-set-faces 'sesame faces))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sesame)

;;; sesame-theme.el ends here
