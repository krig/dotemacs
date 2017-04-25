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

(let ((class '((class color) (min-colors 89)))
      (dark "#222222")
      (light "#fcfcfc")
      (match-bg "#f1c40f")
      (shade-bg "#eeeeec"))
  (custom-theme-set-faces
   'sesame
   `(default ((,class (:foreground ,dark :background ,light))))
   `(mouse ((,class (:foreground ,dark))))
   `(cursor ((,class (:background ,dark))))
   `(border ((,class (:foreground ,dark))))
   `(show-paren-match ((,class (:foreground ,dark :background ,match-bg))))
   `(match ((,class (:foreground ,light :background ,match-bg))))
   `(isearch ((,class (:foreground ,light :background ,dark))))
   `(isearch-lazy-highlight-face ((,class (:foreground ,dark :background ,match-bg))))
   `(notmuch-wash-cited-text ((,class (:foreground ,dark :background ,shade-bg))))
   `(widget-field ((,class (:foreground ,dark :background ,shade-bg))))))

(provide-theme 'sesame)

;;; sesame-theme.el ends here
