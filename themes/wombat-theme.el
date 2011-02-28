;;; wombat-theme.el --- Custom face theme for Emacs

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

(deftheme wombat
  "Theme for faces, based on the Wombat theme from Vim.
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Wombat palette colors.
      ;; f6f3e8
      (wombat-fg "#edebef") (wombat-bg "#1a1a1a");;"#242424")
      (wombat-green "#95e454") (wombat-green+1 "#cae682") (wombat-green+2 "#4BC98A")
      (wombat-pink "#e5786d") (wombat-red "#e5786d")
      (wombat-blue-2 "#2e3436") (wombat-blue-1 "#64a8d8") (wombat-blue "#8ac6f2")
      (wombat-magenta "#cc99cc")
      (wombat-orange-1 "#f57900") (wombat-orange "#e65c00")
      (wombat-orange+1 "#e9b96e") (wombat-orange+2 "#ffc125")
      (wombat-purple-1 "#ad7fa8") (wombat-purple "#cc99cc")
      (wombat-pink-1 "#f283b6") (wombat-pink "#F6B3DF")
      (wombat-gray-1 "#444444") (wombat-gray "#424242") (wombat-gray+1 "#99968b")

      (cursorline-bg "#2d2d2d")
      (cursorcolumn-bg "#2d2d2d")
      (matchparen-fg "#f6f3e8") (matchparen-bg "#857b6f")
      (pmenu-fg "#f6f3e8") (pmenu-bg "#444444")
      (pmenusel-fg "#000000") (pmenu-bg "#cae682")
      (normal-fg "#f6f3e8") (normal-bg "#242424")
      (cursor-bg "#656565")
      (nontext-fg "#808080") (nontext-bg "#303030")
      (linenr-fg "#857b6f") (linenr-bg "#303030")
      (statusline-fg "#f6f3e8") (statusline-bg "#444444")
      (statuslinenc-fg "#857b6f") (statuslinenc-bg "#444444")
      (vertsplit-fg "#444444") (vertsplit-bg "#444444")
      (folded-fg "#a0a8b0") (folded-bg "#384048")
      (title-fg "#f6f3e8")
      (visual-fg "#f6f3e8") (visual-bg "#444444")
      (specialkey-fg "#808080") (specialkey-bg "#343434")
      (comment-fg "#99968b")
      (todo-fg "#8f8f8f")
      (constant-fg "#e5786d")
      (string-fg "#95e454")
      (identifier-fg "#cae682")
      (function-fg "#cae682")
      (type-fg "#cae682")
      (statement-fg "#cae682")
      (keyword-fg "#8ac6f2")
      (preproc-fg "#e5786d")
      (number-fg "#e5786d")
      (special-fg "#e7f6da"))

  (custom-theme-set-faces
   'wombat
   `(default ((,class (:foreground ,wombat-fg :background ,wombat-bg))))
   `(cursor ((,class (:foreground ,wombat-fg :background ,cursor-bg))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,wombat-fg))))
   `(highlight ((,class (:background ,wombat-gray+1))))
   `(region ((,class (:background ,wombat-gray+1))))
   `(secondary-selection ((,class (:background ,wombat-blue))))
   `(isearch ((,class (:foreground ,"#ffffff" :background ,wombat-orange))))
   `(lazy-highlight ((,class (:background ,wombat-blue))))
   `(trailing-whitespace ((,class (:background ,wombat-pink))))
   ;; Mode line faces
   `(mode-line ((,class (:box nil
			 :background "#3c3c3c" :foreground ,wombat-gray+1))))
   `(mode-line-inactive ((,class (:box nil
				  :background "#2a2a2a" :foreground "#555555"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,wombat-blue))))
   `(escape-glyph ((,class (:foreground ,wombat-red))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:weight normal :foreground ,wombat-red))))
   `(font-lock-comment-face ((,class (:italic t :foreground ,wombat-gray-1))))
   `(font-lock-constant-face ((,class (:weight bold :foreground ,wombat-red))))
   `(font-lock-function-name-face ((,class (:foreground ,wombat-green+1))))
   `(font-lock-keyword-face ((,class (:weight normal :underline nil :foreground ,wombat-blue))))
   `(font-lock-string-face ((,class (:italic t :foreground ,wombat-green))))
   `(font-lock-type-face ((,class (:foreground ,wombat-blue-1))))
   `(font-lock-variable-name-face ((,class (:weight bold :foreground ,wombat-fg))))
   `(font-lock-warning-face ((,class (:foreground ,wombat-red))))
   ;; Button and link faces
   `(button ((,class (:underline t :foreground ,wombat-blue))))
   `(link ((,class (:underline t :foreground ,wombat-blue))))
   `(link-visited ((,class (:underline t :foreground ,wombat-blue))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground ,wombat-pink))))
   `(gnus-group-news-1-low ((,class (:foreground ,wombat-pink))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground ,wombat-blue))))
   `(gnus-group-news-2-low ((,class (:foreground ,wombat-blue))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground ,"#4e0a06"))))
   `(gnus-group-news-3-low ((,class (:foreground ,"#4e0a06"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground ,"#7a4c02"))))
   `(gnus-group-news-4-low ((,class (:foreground ,"#7a4c02"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground ,wombat-orange+2))))
   `(gnus-group-news-5-low ((,class (:foreground ,wombat-orange+2))))
   `(gnus-group-news-low ((,class (:foreground ,"#888a85"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground ,wombat-pink))))
   `(gnus-group-mail-1-low ((,class (:foreground ,wombat-pink))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground ,wombat-blue))))
   `(gnus-group-mail-2-low ((,class (:foreground ,wombat-blue))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground ,"#4e0a06"))))
   `(gnus-group-mail-3-low ((,class (:foreground ,"#4e0a06"))))
   `(gnus-group-mail-low ((,class (:foreground ,"#888a85"))))
   `(gnus-header-content ((,class (:foreground ,wombat-green))))
   `(gnus-header-from ((,class (:weight bold :foreground ,wombat-orange))))
   `(gnus-header-subject ((,class (:foreground ,"#4e0a06"))))
   `(gnus-header-name ((,class (:foreground ,wombat-blue))))
   `(gnus-header-newsgroups ((,class (:foreground ,"#888a85"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,wombat-blue))))
   `(message-header-cc ((,class (:foreground ,wombat-orange))))
   `(message-header-other ((,class (:foreground ,wombat-blue-2))))
   `(message-header-subject ((,class (:foreground ,"#4e0a06"))))
   `(message-header-to ((,class (:weight bold :foreground ,wombat-orange))))
   `(message-cited-text ((,class (:foreground ,"#888a85"))))
   `(message-separator ((,class (:weight bold :foreground ,wombat-green))))
   ;; SMerge
   `(smerge-refined-change ((,class (:background ,wombat-purple))))
   ;; Ediff
   `(ediff-current-diff-A ((,class (:background ,wombat-blue))))
   `(ediff-fine-diff-A ((,class (:background ,wombat-purple))))
   `(ediff-current-diff-B ((,class (:background ,wombat-orange-1))))
   `(ediff-fine-diff-B ((,class (:background ,wombat-orange+1))))
   ;; Flyspell
   `(flyspell-duplicate ((,class (:underline ,wombat-orange+1))))
   `(flyspell-incorrect ((,class (:underline ,wombat-pink))))
   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline  ,wombat-green))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,wombat-fg))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,wombat-fg))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,wombat-blue-1))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:underline  ,wombat-orange+2))))
   `(semantic-tag-boundary-face ((,class (:overline   ,wombat-blue))))
   `(semantic-unmatched-syntax-face ((,class (:underline  ,wombat-pink))))

   ;; Trailing whitespace
   `(trailing-whitespace ((t (:background ,wombat-red))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,wombat-green+1))))
   `(ido-only-match ((t (:weight bold :foreground ,wombat-green))))
   `(ido-subdir ((t (:foreground ,wombat-purple))))
   `(ido-virtual ((t (:foreground ,wombat-orange))))
   `(ido-indicator ((t (:foreground ,wombat-blue))))
   `(ido-incomplete-regexp ((t (:foreground ,wombat-red)))))

  (custom-theme-set-variables
   'wombat
   `(ansi-color-names-vector [,wombat-gray-1 ,wombat-red ,wombat-green ,wombat-orange
				      ,wombat-blue ,wombat-pink ,wombat-blue ,wombat-fg])))

(provide-theme 'wombat)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; wombat-theme.el  ends here
