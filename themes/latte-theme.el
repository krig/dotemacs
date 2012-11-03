;;; latte.el --- Custom face theme for Emacs

;; Released under Public Domain by Kristoffer Grönlund.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme latte
  "Cosmic latte theme")

(let
    ((white "#FFFFFF")
     (warning-red "#F0A0A0")
     (cosmic-latte "#FFF8E7")
     (cream "#F4F1DD")
     (pale-latte "#ECE0BA")
     (weak-latte "#ECE0CA")
     (brown-latte "#CC9966")
     (smooth-text "#202020")
     (light-text "#444444")
     (faded-text "#5F5A50")
     (black "#000000")
     (anti-flash-white "#F2F3F4")
     (vanilla "#F3E5AB")
     (pink "#FF0066")
     (blue "#0066FF")
     (dark-green "#2f3f2f")
     (mid-green "#5f7f5f")
     (light-green "#afd8af")
     (dark-mocca "#48321E")
     (light-mocca "#5c3b0f")
     (deep-red "#330000")
     (deep-mocca "#231300")
     (cosmic-turquoise "#9CFFCE"))
  (custom-theme-set-faces
   'latte
   `(default ((t (:background ,cosmic-latte :foreground ,smooth-text))))
   `(escape-glyph ((t (:foreground ,warning-red))))
   `(trailing-whitespace ((t (:background ,warning-red))))
   `(cursor ((t (:background ,smooth-text :foreground ,cosmic-latte))))
   `(highlight ((t (:background ,cosmic-turquoise))))
   `(secondary-selection ((t (:background ,cosmic-turquoise))))
   `(minibuffer-prompt ((t (:foreground ,smooth-text :background ,cosmic-turquoise))))
   `(highlight ((t (:background ,cosmic-turquoise))))
   `(region ((t (:background ,light-green))))
   `(shadow ((t (:foreground ,light-text))))
   `(fringe ((t nil)))
   `(mode-line ((t (:background ,pale-latte :foreground ,dark-mocca))))
   `(mode-line-inactive ((t (:background ,weak-latte :foreground ,faded-text))))
   `(mode-line-highlight ((t (:box (:line-width 2 :color ,warning-red :style released-button)))))
   `(isearch ((t (:background ,cosmic-turquoise :foreground ,pink :weight bold))))
   `(link ((t (:foreground ,blue :underline t))))
   `(link-visited ((t (:inherit link :foreground ,warning-red))))
   `(font-lock-builtin-face ((t (:inherit default :foreground ,deep-red :weight bold))))
   `(font-lock-comment-delimiter-face ((t (:inherit default :foreground ,faded-text))))
   `(font-lock-comment-face ((t (:inherit font-lock-comment-delimiter-face))))
   `(font-lock-constant-face ((t (:inherit default))))
   `(font-lock-function-name-face ((t (:inherit default))))
   `(font-lock-keyword-face ((t (:inherit default :foreground ,smooth-text :weight bold))))
   `(font-lock-preprocessor-face ((t (:inherit default))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit default))))
   `(font-lock-regexp-grouping-construct ((t (:inherit default))))
   `(font-lock-string-face ((t (:inherit default :foreground ,light-mocca))))
   `(font-lock-type-face ((t (:inherit default))))
   `(font-lock-variable-name-face ((t (:inherit default))))
   `(font-lock-warning-face ((t (:background ,warning-red :inherit default))))
   ))

(provide-theme 'latte)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; latte.el  ends here
