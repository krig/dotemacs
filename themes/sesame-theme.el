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
      (success-fg "#278e40")
      (warning-fg "#e74c3c")
      (error-fg "#c0392b")
      (match-bg "#f1c40f")
      (shade-bg "#ecf0f1")
      (hint-bg "#f6f6f6")
      (midnight-blue "#2c3e50")
      (lime "#CDDC39")
      (light-green "#8BC34A")
      (dodger-blue "#19B5FE")
      (hanada "#044F67")
      (watermelon "#ff3b3f")
      (eggplant "#49274a"))
  (custom-theme-set-faces
   'sesame
   `(default ((,class (:foreground ,dark :background ,light))))
   `(cursor ((,class (:background ,dark))))
   ;; Highlighting
   `(fringe ((,class (:background ,light))))
   `(highlight ((,class (:background ,match-bg))))
   `(region ((,class (:background ,shade-bg))))
   `(secondary-selection ((,class (:background ,shade-bg))))
   `(lazy-highlight ((,class (:background ,match-bg))))
   `(trailing-whitespace ((,class (:background ,watermelon))))
   `(isearch ((,class (:foreground ,light :background ,dark))))
   `(isearch-lazy-highlight-face ((,class (:foreground ,dark :background ,match-bg))))
   `(mouse ((,class (:foreground ,dark))))
   `(border ((,class (:foreground ,dark))))
   `(hl-line ((,class (:background ,match-bg :underline t))))
   `(show-paren-match ((,class (:foreground ,dark :background ,match-bg))))
   `(match ((,class (:foreground ,light :background ,match-bg))))
   `(notmuch-search-unread-face ((,class (:foreground ,success-fg :background ,light))))

   ;; Font lock
   `(font-lock-builtin-face ((,class (:foreground ,midnight-blue :background ,light))))
   `(font-lock-comment-face ((,class (:slant italic :foreground ,dark))))
   `(font-lock-constant-face ((,class (:foreground ,dark))))
   `(font-lock-function-name-face ((,class (:foreground ,dark :background ,light :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,dark))))
   `(font-lock-keyword-face ((,class (:foreground ,dark))))
   `(font-lock-string-face ((,class (:foreground ,eggplant))))
   `(font-lock-variable-name-face ((,class (:foreground ,dark :weight bold))))

   ;; Other stuff
   `(trailing-whitespace ((,class (:foreground ,watermelon))))
   `(whitespace-trailing ((,class (:foreground ,watermelon))))
   `(notmuch-wash-cited-text ((,class (:foreground ,dark :background ,shade-bg))))
   `(widget-field ((,class (:foreground ,dark :background ,shade-bg))))))

(provide-theme 'sesame)

;;; sesame-theme.el ends here
