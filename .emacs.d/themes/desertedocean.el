;;; zenburn.el --- just some alien fruit salad to keep you in the zone
;; Copyright (C) 2003, 2004, 2005, 2006  Daniel Brockman

;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/desertedocean/desertedocean.el
;; Updated: 2008-06-23 12:03

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some packages ship with broken implementations of `format-spec';
;; for example, stable versions of TRAMP and ERC do this.  To fix
;; this, you can put the following at the end of your ~/.emacs:

;;   (unless (desertedocean-format-spec-works-p)
;;     (desertedocean-define-format-spec))

;; Thanks to Jani Nurminen, who created the original desertedocean color
;; theme for vim.  I'm just copying him. :-)

;;; Short-Term Wishlist:

;; Theme the ansi-term faces `term-red', etc., and the ERC faces
;; `fg:erc-color-face1', etc.

;; Theme `gnus-server-offline-face', `gnus-server-opened-face', and
;; `gnus-server-denied-face'.  First, find out what they hell they do.

;; Theme `gnus-emphasis-highlight-words' after finding out what it
;; does.

;; Theme `emms-stream-name-face' and `emms-stream-url-face'.

;; Theme `ido-indicator-face'.

;;; Code:

(require 'color-theme)

(defvar desertedocean-fg "#FFE0FA")
(defvar desertedocean-bg "#122130")

(defvar desertedocean-comment "#6aa0e0")
(defvar desertedocean-title "#00abdf")
(defvar desertedocean-underlined "#20b0ef")
(defvar desertedocean-statement "#ef7a7a")
(defvar desertedocean-type "#daa0b0")
(defvar desertedocean-preproc "#ff7a9a")
(defvar desertedocean-constant "#ee8ab5")
(defvar desertedocean-identifier "#ffe0bd")
(defvar desertedocean-special "#8cf0ff")
(defvar desertedocean-ignore "#666666")
(defvar desertedocean-todo-fg "#ee7010")
(defvar desertedocean-todo-bg "#eec90e")
(defvar desertedocean-cursor-fg "#00d0d0")
(defvar desertedocean-cursor-bg "#007799")
(defvar desertedocean-directory "#bbd0df")
(defvar desertedocean-
hi VertSplit    guibg=#c2bfa5 guifg=grey50 gui=none
hi Folded   guibg=#337799 guifg=#BBDDCC
hi FoldColumn   guibg=#337799 guifg=#00CCFF
hi LineNr   guifg=#CCF0FF guibg=#006688 
hi ModeMsg  guifg=#00AACC
hi MoreMsg  guifg=SeaGreen
hi NonText  guifg=#285960 guibg=#2A374A
hi Question guifg=#AABBCC
hi Search   guibg=slategrey guifg=#FFDABB
hi IncSearch    guifg=slategrey guibg=#FFDFB0
hi SpecialKey   guifg=#00CCBB " blue green
hi StatusLine   guibg=#00A5EA guifg=#050709 gui=none
hi StatusLineNC guibg=#1079B0 guifg=#272334 gui=none
hi Visual   guifg=#008FBF guibg=#33DFEF
hi WarningMsg   guifg=salmon
hi Pmenu    guifg=#6Aa0e0 guibg=#222f3d
hi PmenuSel guifg=#FFFFFF guibg=#0088bb 

(defvar font-lock-pseudo-keyword-face 'font-lock-pseudo-keyword-face)
(defvar font-lock-operator-face 'font-lock-operator-face)

(defun desertedocean-format-spec-works-p ()
  (and (fboundp 'format-spec)
       (= (next-property-change
           0 (format-spec #("<%x>" 0 4 (face (:weight bold)))
                          '((?x . "foo"))) 4) 4)))

(defun desertedocean-format-spec (format specification)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
       ;; Quoted percent sign.
       ((eq (char-after) ?%)
        (delete-char 1))
       ;; Valid format spec.
       ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
        (let* ((num (match-string 1))
               (spec (string-to-char (match-string 2)))
               (val (cdr (assq spec specification))))
          (unless val
            (error "Invalid format character: %s" spec))
          (let ((text (format (concat "%" num "s") val)))
            (insert-and-inherit text)
            ;; Delete the specifier body.
            (delete-region (+ (match-beginning 0) (length text))
                           (+ (match-end 0) (length text)))
            ;; Delete the percent sign.
            (delete-region (1- (match-beginning 0)) (match-beginning 0)))))
       ;; Signal an error on bogus format strings.
       (t
        (error "Invalid format string"))))
    (buffer-string)))

(defun desertedocean-define-format-spec ()
  (interactive)
  (fset 'format-spec #'desertedocean-format-spec))

(unless (desertedocean-format-spec-works-p)
  (desertedocean-define-format-spec))

(eval-after-load 'format-spec
  (unless (desertedocean-format-spec-works-p)
    (desertedocean-define-format-spec)))

(setq-default mode-line-buffer-identification
              (list (propertize "%12b" 'face
                                (list :weight 'bold
                                      :foreground desertedocean-yellow))))
(setq-default mode-line-frame-identification "")
(setq-default erc-mode-line-format
              (concat (propertize "%t" 'face
                                  (list :weight 'bold
                                        :foreground desertedocean-yellow))
                      " %a"))

(setq gnus-logo-colors `(,desertedocean-bg+2 ,desertedocean-bg+1)
      gnus-mode-line-image-cache
      '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    11        2            1\",
/* colors */
\". c #dcdccc\",
\"# c None s None\",
/* pixels */
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\"};"))

(defun desertedocean-make-face-alias-clauses (alias-symbols)
  (let (clauses)
    (dolist (alias-symbol alias-symbols clauses)
      (let ((alias-name (symbol-name alias-symbol)))
        (if (not (string-match "-face" alias-name))
            (error "Invalid face alias: %s" alias-name)
          (let ((target-name (replace-regexp-in-string
                              ".*\\(-face\\)" ""
                              alias-name nil nil 1)))
            (push `(,(intern alias-name)
                    ((t (:inherit ,(intern target-name)))))
                  clauses)))))))

;;;###autoload
(defun color-theme-desertedocean ()
  "Just some alien fruit salad to keep you in the zone."
  (interactive)
  (color-theme-install
   (append
    (list 'color-theme-desertedocean
          `((background-color . ,desertedocean-bg)
            (background-mode . dark)
            (border-color . ,desertedocean-bg)
            (foreground-color . ,desertedocean-fg)
            (mouse-color . ,desertedocean-fg))
          `((emms-mode-line-icon-color . ,desertedocean-fg)
            (goto-address-mail-face . italic)
            (goto-address-mail-mouse-face . secondary-selection)
            (goto-address-url-face . bold)
            (goto-address-url-mouse-face . hover-highlight)
            (help-highlight-face . hover-highlight)
            (imaxima-label-color . ,desertedocean-yellow)
            (imaxima-equation-color . ,desertedocean-fg)
            (list-matching-lines-face . bold)
            (view-highlight-face . hover-highlight)
            (widget-mouse-face . hover-highlight))

     '(bold ((t (:weight bold))))
     '(bold-italic ((t (:italic t :weight bold))))
     `(default ((t (:background ,desertedocean-bg :foreground ,desertedocean-fg))))
     '(fixed-pitch ((t (:weight bold))))
     '(italic ((t (:slant italic))))
     '(underline ((t (:underline t))))
     ;; '(variable-pitch ((t (:font "-*-utopia-regular-r-*-*-12-*-*-*-*-*-*-*"))))

     `(desertedocean-background-1 ((t (:background ,desertedocean-bg+1))))
     `(desertedocean-background-2 ((t (:background ,desertedocean-bg+2))))

     `(desertedocean-primary-1 ((t (:foreground ,desertedocean-yellow :weight bold))))
     `(desertedocean-primary-2 ((t (:foreground ,desertedocean-orange :weight bold))))
     '(desertedocean-primary-3 ((t (:foreground "#dfdfbf" :weight bold))))
     '(desertedocean-primary-4 ((t (:foreground "#dca3a3" :weight bold))))
     '(desertedocean-primary-5 ((t (:foreground "#94bff3" :weight bold))))

     '(desertedocean-highlight-damp
       ((t (:foreground "#88b090" :background "#2e3330"))))
     '(desertedocean-highlight-alerting
       ((t (:foreground "#e37170" :background "#332323"))))
     '(desertedocean-highlight-subtle
       ((t (:background "#464646"))))

     '(desertedocean-lowlight-1 ((t (:foreground "#606060"))))
     '(desertedocean-lowlight-2 ((t (:foreground "#708070"))))

     `(desertedocean-yellow ((t (:foreground ,desertedocean-yellow))))
     `(desertedocean-orange ((t (:foreground ,desertedocean-orange))))
     `(desertedocean-red ((t (:foreground ,desertedocean-red))))
     `(desertedocean-green-1 ((t (:foreground ,desertedocean-green-1))))
     `(desertedocean-green ((t (:foreground ,desertedocean-green))))
     `(desertedocean-green+1 ((t (:foreground ,desertedocean-green+1))))
     `(desertedocean-green+2 ((t (:foreground ,desertedocean-green+2))))
     `(desertedocean-green+3 ((t (:foreground ,desertedocean-green+3))))
     `(desertedocean-green+4 ((t (:foreground ,desertedocean-green+4))))
     `(desertedocean-blue ((t (:foreground ,desertedocean-blue))))
     `(desertedocean-blue-1 ((t (:foreground ,desertedocean-blue-1))))
     `(desertedocean-blue-2 ((t (:foreground ,desertedocean-blue-2))))
     `(desertedocean-blue-3 ((t (:foreground ,desertedocean-blue-3))))
     `(desertedocean-blue-4 ((t (:foreground ,desertedocean-blue-4))))

     '(desertedocean-title ((t (:inherit variable-pitch :weight bold))))

     '(font-lock-builtin
       ((t (:inherit desertedocean-blue))))
     '(font-lock-comment
       ((t (:inherit desertedocean-green))))
     '(font-lock-comment-delimiter
       ((t (:inherit desertedocean-lowlight-2))))
     '(font-lock-constant
       ((t (:inherit desertedocean-primary-4))))
     '(font-lock-doc
       ((t (:inherit desertedocean-green+1))))
     `(font-lock-function-name
       ((t (:foreground ,desertedocean-yellow))))
     '(font-lock-keyword
       ((t (:inherit desertedocean-primary-1))))
     '(font-lock-negation-char
       ((t (:inherit desertedocean-primary-1))))
     '(font-lock-preprocessor
       ((t (:inherit desertedocean-blue))))
     '(font-lock-string
       ((t (:inherit desertedocean-red))))
     '(font-lock-type
       ((t (:inherit desertedocean-primary-3))))
     `(font-lock-variable-name
       ((t (:foreground ,desertedocean-yellow))))
     '(font-lock-warning
       ((t (:inherit desertedocean-highlight-alerting))))

     '(font-lock-pseudo-keyword
       ((t (:inherit desertedocean-primary-2))))
     '(font-lock-operator
       ((t (:inherit desertedocean-primary-3))))

     '(term-default-bg ((t (nil))))
     '(term-default-bg-inv ((t (nil))))
     '(term-default-fg ((t (nil))))
     '(term-default-fg-inv ((t (nil))))
     '(term-invisible ((t (nil)))) ;; FIXME: Security risk?
     '(term-invisible-inv  ((t (nil))))
     '(term-bold ((t (:weight bold))))
     '(term-underline ((t (:underline t))))

     ;; FIXME: Map these to ansi-term's faces (`term-red', etc.).
     '(desertedocean-term-dark-gray      ((t (:foreground "#709080"))))
     '(desertedocean-term-light-blue     ((t (:foreground "#94bff3"))))
     '(desertedocean-term-light-cyan     ((t (:foreground "#93e0e3"))))
     '(desertedocean-term-light-green    ((t (:foreground "#c3bf9f"))))
     '(desertedocean-term-light-magenta  ((t (:foreground "#ec93d3"))))
     '(desertedocean-term-light-red      ((t (:foreground "#dca3a3"))))
     '(desertedocean-term-light-yellow   ((t (:foreground "#f0dfaf"))))
     '(desertedocean-term-white          ((t (:foreground "#ffffff"))))

     '(desertedocean-term-black          ((t (:foreground "#000000"))))
     '(desertedocean-term-dark-blue      ((t (:foreground "#506070"))))
     '(desertedocean-term-dark-cyan      ((t (:foreground "#8cd0d3"))))
     '(desertedocean-term-dark-green     ((t (:foreground "#60b48a"))))
     '(desertedocean-term-dark-magenta   ((t (:foreground "#dc8cc3"))))
     '(desertedocean-term-dark-red       ((t (:foreground "#705050"))))
     '(desertedocean-term-dark-yellow    ((t (:foreground "#dfaf8f"))))
     `(desertedocean-term-light-gray     ((t (:foreground ,desertedocean-fg))))

     '(plain-widget-button
       ((t (:weight bold))))
     '(plain-widget-button-pressed
       ((t (:inverse-video t))))
     '(plain-widget-documentation
       ((t (:inherit font-lock-doc))))
     `(plain-widget-field
       ((t (:background ,desertedocean-bg+2))))
     '(plain-widget-inactive
       ((t (:strike-through t))))
     `(plain-widget-single-line-field
       ((t (:background ,desertedocean-bg+2))))

     `(fancy-widget-button
       ((t (:background ,desertedocean-bg+1
            :box (:line-width 2 :style released-button)))))
     `(fancy-widget-button-pressed
       ((t (:background ,desertedocean-bg+1
            :box (:line-width 2 :style pressed-button)))))
     `(fancy-widget-button-highlight
       ((t (:background ,desertedocean-bg+1
            :box (:line-width 2 :style released-button)))))
     `(fancy-widget-button-pressed-highlight
       ((t (:background ,desertedocean-bg+1
            :box (:line-width 2 :style pressed-button)))))
     '(fancy-widget-documentation
       ((t (:inherit font-lock-doc))))
     `(fancy-widget-field
       ((t (:background ,desertedocean-bg+2))))
     '(fancy-widget-inactive
       ((t (:strike-through t))))
     `(fancy-widget-single-line-field
       ((t (:background ,desertedocean-bg+2))))

     '(widget-button
       ((t (:inherit plain-widget-button))))
     '(widget-button-pressed
       ((t (:inherit fancy-widget-button-pressed))))
     '(widget-button-highlight
       ((t (:inherit fancy-widget-button-highlight))))
     '(widget-button-pressed-highlight
       ((t (:inherit fancy-widget-button-pressed-highlight))))
     '(widget-documentation
       ((t (:inherit fancy-widget-documentation))))
     '(widget-field
       ((t (:inherit fancy-widget-field))))
     '(widget-inactive
       ((t (:inherit fancy-widget-inactive))))
     '(widget-single-line-field
       ((t (:inherit fancy-widget-single-line-field))))

     `(border ((t (:background ,desertedocean-bg))))
     '(fringe ((t (:inherit desertedocean-highlight-subtle))))
     '(header-line ((t (:inherit desertedocean-highlight-damp))))
     '(mode-line ((t (:background "#353b37" :foreground "#acbc90"
                      :box (:color "#353b37" :line-width 2)))))
     '(mode-line-inactive ((t (:background "#2e3330" :foreground "#88b090"
                               :box (:color "#2e3330" :line-width 2)))))
     `(minibuffer-prompt ((t (:foreground ,desertedocean-yellow))))
     `(Buffer-menu-buffer ((t (:inherit desertedocean-primary-1))))

     '(region ((t (:foreground "#71d3b4" :background "#233323"))))
     `(secondary-selection ((t (:foreground ,desertedocean-fg :background "#506070"))))

     '(trailing-whitespace ((t (:inherit font-lock-warning))))
     '(highlight ((t (:inherit font-lock-warning))))
     '(paren ((t (:inherit desertedocean-lowlight-1))))
     '(show-paren-mismatch ((t (:inherit font-lock-warning))))
     '(show-paren-match ((t (:inherit font-lock-keyword))))
     '(match ((t (:weight bold))))

     `(cursor ((t (:background ,desertedocean-fg :foreground ,desertedocean-bg))))
     '(hover-highlight ((t (:underline t :foreground "#f8f893"))))
     '(menu ((t nil)))
     '(mouse ((t (:inherit desertedocean-foreground))))
     `(scroll-bar ((t (:background ,desertedocean-bg+2))))
     `(tool-bar ((t (:background ,desertedocean-bg+2))))

     '(ido-first-match ((t (:inherit desertedocean-primary-1))))
     '(ido-only-match ((t (:inherit desertedocean-primary-2))))
     `(ido-subdir ((t (:foreground ,desertedocean-yellow))))

     `(isearch ((t (:foreground ,desertedocean-fg :background "#506070"))))
     `(isearch-lazy-highlight
       ((t (:foreground ,desertedocean-fg :background "#1e2320" :weight normal))))

     '(mtorus-highlight ((t (:inherit desertedocean-highlight-bluish))))
     '(mtorus-notify-highlight ((t (:inherit desertedocean-primary-1))))

     '(which-func ((t (:inherit mode-line))))

     '(apt-utils-normal-package
       ((t (:inherit desertedocean-primary-1))))
     '(apt-utils-virtual-package
       ((t (:inherit desertedocean-primary-2))))
     '(apt-utils-field-keyword
       ((t (:inherit font-lock-doc))))
     '(apt-utils-field-contents
       ((t (:inherit font-lock-comment))))
     '(apt-utils-summary
       ((t (:inherit bold))))
     '(apt-utils-description
       ((t (:inherit default))))
     '(apt-utils-version
       ((t (:inherit desertedocean-blue))))
     '(apt-utils-broken
       ((t (:inherit font-lock-warning))))

     '(breakpoint-enabled-bitmap ((t (:inherit desertedocean-primary-1))))
     '(breakpoint-disabled-bitmap ((t (:inherit font-lock-comment))))

     '(calendar-today ((t (:underline nil :inherit desertedocean-primary-2))))
     '(diary ((t (:underline nil :inherit desertedocean-primary-1))))
     '(holiday ((t (:underline t :inherit desertedocean-primary-4))))

     '(change-log-date ((t (:inherit desertedocean-blue))))

     '(comint-highlight-input ((t (:inherit desertedocean-primary-1))))
     '(comint-highlight-prompt ((t (:inherit desertedocean-primary-2))))

     '(compilation-info ((t (:inherit desertedocean-primary-1))))
     '(compilation-warning ((t (:inherit font-lock-warning))))

     ;; TODO
     '(cua-rectangle ((t (:inherit region))))

     '(custom-button
       ((t (:inherit fancy-widget-button))))
     '(custom-button-pressed
       ((t (:inherit fancy-widget-button-pressed))))
     '(custom-changed
       ((t (:inherit desertedocean-blue))))
     '(custom-comment
       ((t (:inherit font-lock-doc))))
     '(custom-comment-tag
       ((t (:inherit font-lock-doc))))
     '(custom-documentation
       ((t (:inherit font-lock-doc))))
     '(custom-tag
       ((t (:inherit desertedocean-primary-2))))
     '(custom-group-tag
       ((t (:inherit desertedocean-primary-1))))
     '(custom-group-tag-1
       ((t (:inherit desertedocean-primary-4))))
     '(custom-invalid
       ((t (:inherit font-lock-warning))))
     '(custom-modified
       ((t (:inherit desertedocean-primary-3))))
     '(custom-rogue
       ((t (:inhrit font-lock-warning))))
     '(custom-saved
       ((t (:underline t))))
     '(custom-set
       ((t (:inverse-video t :inherit desertedocean-blue))))
     '(custom-state
       ((t (:inherit font-lock-comment))))
     '(custom-variable-button
       ((t (:weight bold :underline t))))
     '(custom-variable-tag
       ((t (:inherit desertedocean-primary-2))))

     '(dictionary-button ((t (:inherit fancy-widget-button))))
     '(dictionary-reference ((t (:inherit desertedocean-primary-1))))
     '(dictionary-word-entry ((t (:inherit font-lock-keyword))))

     '(diff-header ((t (:inherit desertedocean-highlight-subtle))))
     '(diff-index ((t (:inherit bold))))
     '(diff-file-header ((t (:inherit bold))))
     '(diff-hunk-header ((t (:inherit desertedocean-highlight-subtle))))

     '(diff-added ((t (:inherit desertedocean-primary-3))))
     '(diff-removed ((t (:inherit desertedocean-blue))))
     '(diff-context ((t (:inherit font-lock-comment))))

     `(emms-pbi-song ((t (:foreground ,desertedocean-yellow))))
     '(emms-pbi-current ((t (:inherit desertedocean-primary-1))))
     '(emms-pbi-mark-marked ((t (:inherit desertedocean-primary-2))))

     '(erc-action ((t (:inherit erc-default))))
     '(erc-bold ((t (:weight bold))))
     '(erc-current-nick ((t (:inherit desertedocean-primary-1))))
     '(erc-dangerous-host ((t (:inherit font-lock-warning))))
     `(erc-default ((t (:foreground ,desertedocean-fg))))
     '(erc-direct-msg ((t (:inherit erc-default))))
     '(erc-error ((t (:inherit font-lock-warning))))
     '(erc-fool ((t (:inherit desertedocean-lowlight-1))))
     '(erc-highlight ((t (:inherit hover-highlight))))
     `(erc-input ((t (:foreground ,desertedocean-yellow))))
     '(erc-keyword ((t (:inherit desertedocean-primary-1))))
     '(erc-nick-default ((t (:inherit bold))))
     '(erc-nick-msg ((t (:inherit erc-default))))
     '(erc-notice ((t (:inherit desertedocean-green))))
     '(erc-pal ((t (:inherit desertedocean-primary-3))))
     '(erc-prompt ((t (:inherit desertedocean-primary-2))))
     '(erc-timestamp ((t (:inherit desertedocean-green+1))))
     '(erc-underline ((t (:inherit underline))))

     '(circe-highlight-nick-face ((t (:inherit desertedocean-primary-1))))
     '(circe-my-message-face ((t (:inherit desertedocean-yellow))))
     '(circe-originator-face ((t (:inherit bold))))
     '(circe-prompt-face ((t (:inherit desertedocean-primary-1))))
     '(circe-server-face ((t (:inherit font-lock-comment-face))))

     '(rcirc-my-nick ((t (:inherit desertedocean-primary-1))))
     '(rcirc-other-nick ((t (:inherit bold))))
     '(rcirc-server ((t (:inherit desertedocean-green))))
     '(rcirc-nick-in-message ((t (:inherit bold))))
     '(rcirc-prompt ((t (:inherit desertedocean-primary-1))))
     '(rcirc-mode-line-nick ((t (:inherit desertedocean-primary-1))))

     '(eshell-prompt ((t (:inherit desertedocean-primary-1))))
     '(eshell-ls-archive ((t (:foreground "#c3bf9f" :weight bold))))
     '(eshell-ls-backup ((t (:inherit font-lock-comment))))
     '(eshell-ls-clutter ((t (:inherit font-lock-comment))))
     `(eshell-ls-directory ((t (:foreground ,desertedocean-blue+1 :weight bold))))
     `(eshell-ls-executable ((t (:foreground ,desertedocean-red+1 :weight bold))))
     '(eshell-ls-unreadable ((t (:inherit desertedocean-lowlight-1))))
     '(eshell-ls-missing ((t (:inherit font-lock-warning))))
     '(eshell-ls-product ((t (:inherit font-lock-doc))))
     '(eshell-ls-special ((t (:inherit desertedocean-primary-1))))
     `(eshell-ls-symlink ((t (:foreground ,desertedocean-cyan :weight bold))))

     '(highlight-current-line ((t (:inherit desertedocean-highlight-subtle))))

     '(ibuffer-deletion ((t (:inherit desertedocean-primary-2))))
     '(ibuffer-marked ((t (:inherit desertedocean-primary-1))))
     '(ibuffer-special-buffer ((t (:inherit font-lock-doc))))
     '(ibuffer-help-buffer ((t (:inherit font-lock-comment))))

     '(message-cited-text ((t (:inherit font-lock-comment))))
     ;;`(message-cited-text ((t (:foreground ,desertedocean-blue))))
     '(message-header-name ((t (:inherit desertedocean-green+1))))
     '(message-header-other ((t (:inherit desertedocean-green))))
     '(message-header-to ((t (:inherit desertedocean-primary-1))))
     '(message-header-from ((t (:inherit desertedocean-primary-1))))
     '(message-header-cc ((t (:inherit desertedocean-primary-1))))
     '(message-header-newsgroups ((t (:inherit desertedocean-primary-1))))
     '(message-header-subject ((t (:inherit desertedocean-primary-2))))
     '(message-header-xheader ((t (:inherit desertedocean-green))))
     '(message-mml ((t (:inherit desertedocean-primary-1))))
     '(message-separator ((t (:inherit font-lock-comment))))

     '(gnus-header-name ((t (:inherit message-header-name))))
     '(gnus-header-content ((t (:inherit message-header-other))))
     '(gnus-header-from ((t (:inherit message-header-from))))
     '(gnus-header-subject ((t (:inherit message-header-subject))))
     '(gnus-header-newsgroups ((t (:inherit message-header-other))))

     `(gnus-x-face ((t (:background ,desertedocean-fg :foreground ,desertedocean-bg))))

     ;; (gnus-cite-1 ((t (:inherit message-cited-text))))
     `(gnus-cite-1 ((t (:foreground ,desertedocean-blue))))
     `(gnus-cite-2 ((t (:foreground ,desertedocean-blue-1))))
     `(gnus-cite-3 ((t (:foreground ,desertedocean-blue-2))))
;;      (gnus-cite-4 ((t (:foreground ,desertedocean-blue-3))))
;;      (gnus-cite-5 ((t (:foreground ,desertedocean-blue-4))))
;;      (gnus-cite-6 ((t (:foreground ,desertedocean-red-4))))
;;      (gnus-cite-5 ((t (:foreground ,desertedocean-red-3))))
     `(gnus-cite-4 ((t (:foreground ,desertedocean-green+2))))
     `(gnus-cite-5 ((t (:foreground ,desertedocean-green+1))))
     `(gnus-cite-6 ((t (:foreground ,desertedocean-green))))
     `(gnus-cite-7 ((t (:foreground ,desertedocean-red))))
     `(gnus-cite-8 ((t (:foreground ,desertedocean-red-1))))
     `(gnus-cite-9 ((t (:foreground ,desertedocean-red-2))))
     `(gnus-cite-10 ((t (:foreground ,desertedocean-yellow-1))))
     `(gnus-cite-11 ((t (:foreground ,desertedocean-yellow))))

     '(gnus-group-mail-1 ((t (:inherit desertedocean-primary-1))))
     '(gnus-group-mail-2 ((t (:inherit desertedocean-primary-1))))
     '(gnus-group-mail-3 ((t (:inherit desertedocean-primary-1))))
     '(gnus-group-mail-1-empty ((t (:inherit default))))
     '(gnus-group-mail-2-empty ((t (:inherit default))))
     `(gnus-group-mail-3-empty ((t (:foreground ,desertedocean-yellow))))
     '(gnus-group-news-1-empty ((t (:inherit default))))
     '(gnus-group-news-2-empty ((t (:inherit default))))
     '(gnus-group-news-3-empty ((t (:inherit default))))

     `(gnus-signature ((t (:foreground ,desertedocean-yellow))))

     '(gnus-summary-selected
       ((t (:inherit desertedocean-primary-1))))
     '(gnus-summary-cancelled
       ((t (:inherit desertedocean-highlight-alerting))))

     '(gnus-summary-low-ticked
       ((t (:inherit desertedocean-primary-2))))
     '(gnus-summary-normal-ticked
       ((t (:inherit desertedocean-primary-2))))
     '(gnus-summary-high-ticked
       ((t (:inherit desertedocean-primary-2))))

     '(gnus-summary-low-unread
       ((t (:inherit desertedocean-foreground :weight normal))))
     '(gnus-summary-normal-unread
       ((t (:inherit desertedocean-foreground :weight normal))))
     '(gnus-summary-high-unread
       ((t (:inherit desertedocean-foreground :weight bold))))

     '(gnus-summary-low-read
       ((t (:inherit desertedocean-green :weight normal))))
     '(gnus-summary-normal-read
       ((t (:inherit desertedocean-green :weight normal))))
     '(gnus-summary-high-read
       ((t (:inherit desertedocean-green :weight bold))))

     '(gnus-summary-low-ancient
       ((t (:inherit desertedocean-blue :weight normal))))
     '(gnus-summary-normal-ancient
       ((t (:inherit desertedocean-blue :weight normal))))
     '(gnus-summary-high-ancient
       ((t (:inherit desertedocean-blue))))

     '(help-argument-name ((t (:weight bold))))

     ;; See also the variable definitions at the top of this file
     '(imaxima-latex-error ((t (:inherit font-lock-warning))))

     `(info-xref ((t (:foreground ,desertedocean-yellow :weight bold))))
     '(info-xref-visited ((t (:inherit info-xref :weight normal))))
     '(info-header-xref ((t (:inherit info-xref))))
     `(info-menu-star ((t (:foreground ,desertedocean-orange :weight bold))))
     `(info-menu-5 ((t (:inherit info-menu-star))))
     '(info-node ((t (:weight bold))))
     '(info-header-node ((t (:weight normal))))

     '(jabber-roster-user-chatty
       ((t (:inherit desertedocean-primary-1))))
     '(jabber-roster-user-online
       ((t (:inherit desertedocean-primary-2))))
     '(jabber-roster-user-away
       ((t (:inherit font-lock-doc))))
     '(jabber-roster-user-xa
       ((t (:inherit font-lock-comment))))
     '(jabber-roster-user-offline
       ((t (:inherit desertedocean-lowlight-1))))
     '(jabber-roster-user-dnd
       ((t (:inherit desertedocean-primary-5))))
     '(jabber-roster-user-error
       ((t (:inherit font-lock-warning))))

     '(jabber-title-small
       ((t (:inherit desertedocean-title :height 1.2))))
     '(jabber-title-medium
       ((t (:inherit jabber-title-small :height 1.2))))
     '(jabber-title-large
       ((t (:inherit jabber-title-medium :height 1.2))))

     '(jabber-chat-prompt-local
       ((t (:inherit desertedocean-primary-1))))
     '(jabber-chat-prompt-foreign
       ((t (:inherit desertedocean-primary-2))))

     '(jabber-rare-time-face
       ((t (:inherit desertedocean-green+1))))

     '(jde-java-font-lock-modifier
       ((t (:inherit desertedocean-primary-2))))
     '(jde-java-font-lock-doc-tag
       ((t (:inherit desertedocean-primary-1))))
     '(jde-java-font-lock-constant
       ((t (:inherit font-lock-constant))))
     '(jde-java-font-lock-package
       ((t (:inherit desertedocean-primary-3))))
     '(jde-java-font-lock-number
       ((t (:inherit font-lock-constant))))
     '(jde-java-font-lock-operator
       ((t (:inherit font-lock-keyword))))
     '(jde-java-font-lock-link
       ((t (:inherit desertedocean-primary-5 :underline t))))

     '(semantic-tag-boundary-face
       ((t (:overline "#5f5f5f"))))

     '(keywiz-right ((t (:inherit desertedocean-primary-1))))
     '(keywiz-wrong ((t (:inherit font-lock-warning))))
     '(keywiz-command ((t (:inherit desertedocean-primary-2))))

     '(font-latex-bold ((t (:inherit bold))))
     '(font-latex-warning ((t (:inherit font-lock-warning))))
     '(font-latex-sedate ((t (:inherit desertedocean-primary-1))))
     '(font-latex-title-4 ((t (:inherit desertedocean-title))))

     '(makefile-space ((t (:inherit font-lock-warning))))
     '(makefile-shell ((t (nil))))
     ;; This does not work very well because everything that's highlighted
     ;; inside the shell region will get its own box.
     ;; (makefile-shell ((t (:background "#4f4f4f"
     ;;                           :box (:line-width 2 :color "#4f4f4f")))))

     '(nxml-delimited-data ((t (:inherit font-lock-string))))
     '(nxml-name ((t (:inherit desertedocean-primary-1))))
     '(nxml-ref ((t (:inherit desertedocean-primary-5))))
     '(nxml-delimiter ((t (:inherit default))))
     '(nxml-text ((t (:inherit default))))

     '(nxml-comment-content
       ((t (:inherit font-lock-comment))))
     '(nxml-comment-delimiter
       ((t (:inherit nxml-comment-content))))
     '(nxml-processing-instruction-target
       ((t (:inherit desertedocean-primary-2))))
     '(nxml-processing-instruction-delimiter
       ((t (:inherit nxml-processing-instruction-target))))
     '(nxml-processing-instruction-content
       ((t (:inherit nxml-processing-instruction-target))))
     '(nxml-cdata-section-CDATA
       ((t (:inherit desertedocean-primary-4))))
     '(nxml-cdata-section-delimiter
       ((t (:inherit nxml-cdata-section-CDATA))))
     '(nxml-cdata-section-content
       ((t (:inherit nxml-text))))
     '(nxml-entity-ref-name
       ((t (:inherit desertedocean-primary-5))))
     '(nxml-entity-ref-delimiter
       ((t (:inherit nxml-entity-ref-name))))
     '(nxml-char-ref-number
       ((t (:inherit nxml-entity-ref-name))))
     '(nxml-char-ref-delimiter
       ((t (:inherit nxml-entity-ref-delimiter))))

     '(nxml-tag-delimiter ((t (:inherit default))))
     '(nxml-tag-slash ((t (:inherit default))))
     '(nxml-element-local-name ((t (:inherit desertedocean-primary-1))))
     '(nxml-element-prefix ((t (:inherit default))))
     '(nxml-element-colon ((t (:inherit default))))

     '(nxml-attribute-local-name
       ((t (:inherit desertedocean-primary-3))))
     '(nxml-namespace-attribute-prefix
       ((t (:inherit nxml-attribute-local-name))))
     '(nxml-attribute-value
       ((t (:inherit font-lock-string))))
     '(nxml-attribute-value-delimiter
       ((t (:inherit nxml-attribute-value))))
     '(nxml-attribute-prefix
       ((t (:inherit default))))
     '(nxml-namespace-attribute-xmlns
       ((t (:inherit nxml-attribute-prefix))))
     '(nxml-attribute-colon
       ((t (:inherit default))))
     '(nxml-namespace-attribute-colon
       ((t (:inherit nxml-attribute-colon))))

     ;; TODO
     '(outline-8 ((t (:inherit default))))
     '(outline-7 ((t (:inherit outline-8 :height 1.0))))
     '(outline-6 ((t (:inherit outline-7 :height 1.0))))
     '(outline-5 ((t (:inherit outline-6 :height 1.0))))
     '(outline-4 ((t (:inherit outline-5 :height 1.0))))
     '(outline-3 ((t (:inherit outline-4 :height 1.0))))
     '(outline-2 ((t (:inherit outline-3 :height 1.0))))
     '(outline-1 ((t (:inherit outline-2 :height 1.0))))

     '(setnu-line-number ((t (:inherit desertedocean-lowlight-2))))

     '(speedbar-button ((t (:inherit desertedocean-primary-1))))
     '(speedbar-file ((t (:inherit desertedocean-primary-2))))
     '(speedbar-directory ((t (:inherit desertedocean-primary-5))))
     '(speedbar-tag ((t (:inherit font-lock-function-name))))
     '(speedbar-highlight ((t (:underline t))))

     '(strokes-char ((t (:inherit font-lock-keyword))))

     '(todoo-item-header
       ((t (:inherit desertedocean-primary-1))))
     '(todoo-item-assigned-header
       ((t (:inherit desertedocean-primary-2))))
     `(todoo-sub-item-header
       ((t (:foreground ,desertedocean-yellow))))

     '(tuareg-font-lock-governing
       ((t (:inherit desertedocean-primary-2))))
     '(tuareg-font-lock-interactive-error
       ((t (:inherit font-lock-warning))))
     '(tuareg-font-lock-interactive-output
       ((t (:inherit desertedocean-primary-3))))
     '(tuareg-font-lock-operator
       ((t (:inherit font-lock-operator))))

     '(flymake-errline ((t (:underline "#cc9393"))))
     '(flymake-warnline ((t (:underline "#f0dfaf"))))

     '(tabbar-default-face ((t (:inherit default :background "#353b37" :foreground "#acbc90" :height 0.7))))
     ;;'(mode-line-inactive ((t (:background "#2e3330" :foreground "#88b090"
     ;;                          :box (:color "#2e3330" :line-width 2)))))

     ;;'(tabbar-default-face ((t (:inherit variable-pitch :background "#4f4f4f" :foreground "#999999" :height 0.7))))
     '(tabbar-button-face ((t (:inherit tabbar-default-face))))
     `(tabbar-selected-face ((t (:inherit tabbar-button-face :foreground ,desertedocean-yellow))))
     '(tabbar-unselected-face ((t (:inherit tabbar-button-face))))
     '(trailing-whitespace ((((class color)) (:background "#883030"))))

     '(w3m-form-button
       ((t (:inherit widget-button))))
     '(w3m-form-button-pressed
       ((t (:inherit widget-button-pressed))))
     '(w3m-form-button-mouse
       ((t (:inherit widget-button-pressed))))
     '(w3m-tab-unselected
       ((t (:box (:line-width 1 :style released-button)))))
     '(w3m-tab-selected
       ((t (:box (:line-width 1 :style pressed-button)))))
     '(w3m-tab-unselected-retrieving
       ((t (:inherit (w3m-tab-unselected widget-inactive)))))
     '(w3m-tab-selected-retrieving
       ((t (:inherit (w3m-tab-selected widget-inactive)))))
     '(w3m-tab-background
       ((t (:inherit desertedocean-highlight-subtle))))
     '(w3m-anchor
       ((t (:inherit desertedocean-primary-1))))
     '(w3m-arrived-anchor
       ((t (:inherit desertedocean-primary-2))))
     '(w3m-image
       ((t (:inherit desertedocean-primary-4))))
     '(w3m-form
       ((t (:inherit widget-field)))))

    (desertedocean-make-face-alias-clauses
     '(Buffer-menu-buffer-face
       apt-utils-broken-face
       apt-utils-description-face
       apt-utils-field-contents-face
       apt-utils-field-keyword-face
       apt-utils-normal-package-face
       apt-utils-summary-face
       apt-utils-version-face
       apt-utils-virtual-package-face
       breakpoint-disabled-bitmap-face
       breakpoint-enabled-bitmap-face
       calendar-today-face
       change-log-date-face
       compilation-info-face
       compilation-warning-face
       cua-rectangle-face
       custom-button-face
       custom-button-pressed-face
       custom-changed-face
       custom-comment-face
       custom-comment-tag-face
       custom-documentation-face
       custom-face-tag-face
       custom-group-tag-face
       custom-group-tag-face-1
       custom-invalid-face
       custom-modified-face
       custom-rogue-face
       custom-saved-face
       custom-set-face
       custom-state-face
       custom-variable-button-face
       custom-variable-tag-face
       diary-face
       dictionary-button-face
       dictionary-reference-face
       dictionary-word-entry-face
       diff-added-face
       diff-context-face
       diff-file-header-face
       diff-header-face
       diff-hunk-header-face
       diff-index-face
       diff-removed-face
       emms-pbi-current-face
       emms-pbi-mark-marked-face
       emms-pbi-song-face
       erc-action-face
       erc-bold-face
       erc-current-nick-face
       erc-dangerous-host-face
       erc-default-face
       erc-direct-msg-face
       erc-error-face
       erc-fool-face
       erc-highlight-face
       erc-input-face
       erc-keyword-face
       erc-nick-default-face
       erc-nick-msg-face
       erc-notice-face
       erc-pal-face
       erc-prompt-face
       erc-timestamp-face
       erc-underline-face
       eshell-ls-archive-face
       eshell-ls-backup-face
       eshell-ls-clutter-face
       eshell-ls-directory-face
       eshell-ls-executable-face
       eshell-ls-missing-face
       eshell-ls-product-face
       eshell-ls-special-face
       eshell-ls-symlink-face
       eshell-ls-unreadable-face
       eshell-prompt-face
       fancy-widget-button-face
       fancy-widget-button-highlight-face
       fancy-widget-button-pressed-face
       fancy-widget-button-pressed-highlight-face
       fancy-widget-documentation-face
       fancy-widget-field-face
       fancy-widget-inactive-face
       fancy-widget-single-line-field-face
       font-latex-bold-face
       font-latex-sedate-face
       font-latex-title-4-face
       font-latex-warning-face
       font-lock-builtin-face
       font-lock-comment-delimiter-face
       font-lock-comment-face
       font-lock-constant-face
       font-lock-doc-face
       font-lock-function-name-face
       font-lock-keyword-face
       font-lock-negation-char-face
       font-lock-operator-face
       font-lock-preprocessor-face
       font-lock-pseudo-keyword-face
       font-lock-string-face
       font-lock-type-face
       font-lock-variable-name-face
       font-lock-warning-face
       gnus-cite-face-1
       gnus-cite-face-10
       gnus-cite-face-11
       gnus-cite-face-2
       gnus-cite-face-3
       gnus-cite-face-4
       gnus-cite-face-5
       gnus-cite-face-6
       gnus-cite-face-7
       gnus-cite-face-8
       gnus-cite-face-9
       gnus-group-mail-1-empty-face
       gnus-group-mail-2-empty-face
       gnus-group-mail-3-empty-face
       gnus-group-mail-3-face
       gnus-group-news-1-empty-face
       gnus-group-news-2-empty-face
       gnus-group-news-3-empty-face
       gnus-header-content-face
       gnus-header-from-face
       gnus-header-name-face
       gnus-header-newsgroups-face
       gnus-header-subject-face
       gnus-signature-face
       gnus-summary-cancelled-face
       gnus-summary-high-ancient-face
       gnus-summary-high-read-face
       gnus-summary-high-ticked-face
       gnus-summary-high-unread-face
       gnus-summary-low-ancient-face
       gnus-summary-low-read-face
       gnus-summary-low-ticked-face
       gnus-summary-low-unread-face
       gnus-summary-normal-ancient-face
       gnus-summary-normal-read-face
       gnus-summary-normal-ticked-face
       gnus-summary-normal-unread-face
       gnus-summary-selected-face
       highlight-current-line-face
       holiday-face
       ibuffer-deletion-face
       ibuffer-help-buffer-face
       ibuffer-marked-face
       ibuffer-special-buffer-face
       ido-first-match-face
       ido-only-match-face
       ido-subdir-face
       imaxima-latex-error-face
       isearch-lazy-highlight-face
       jde-java-font-lock-constant-face
       jde-java-font-lock-doc-tag-face
       jde-java-font-lock-link-face
       jde-java-font-lock-modifier-face
       jde-java-font-lock-number-face
       jde-java-font-lock-operator-face
       jde-java-font-lock-package-face
       keywiz-command-face
       keywiz-right-face
       keywiz-wrong-face
       makefile-shell-face
       makefile-space-face
       message-cited-text-face
       message-header-cc-face
       message-header-from-face
       message-header-name-face
       message-header-newsgroups-face
       message-header-other-face
       message-header-subject-face
       message-header-to-face
       message-header-xheader-face
       message-mml-face
       message-separator-face
       mtorus-highlight-face
       mtorus-notify-highlight-face
       nxml-attribute-colon-face
       nxml-attribute-local-name-face
       nxml-attribute-prefix-face
       nxml-attribute-value-delimiter-face
       nxml-attribute-value-face
       nxml-cdata-section-CDATA-face
       nxml-cdata-section-content-face
       nxml-cdata-section-delimiter-face
       nxml-char-ref-delimiter-face
       nxml-char-ref-number-face
       nxml-comment-content-face
       nxml-comment-delimiter-face
       nxml-delimited-data-face
       nxml-delimiter-face
       nxml-element-colon-face
       nxml-element-local-name-face
       nxml-element-prefix-face
       nxml-entity-ref-delimiter-face
       nxml-entity-ref-name-face
       nxml-name-face
       nxml-namespace-attribute-colon-face
       nxml-namespace-attribute-prefix-face
       nxml-namespace-attribute-xmlns-face
       nxml-processing-instruction-content-face
       nxml-processing-instruction-delimiter-face
       nxml-processing-instruction-target-face
       nxml-ref-face
       nxml-tag-delimiter-face
       nxml-tag-slash-face
       nxml-text-face
       paren-face
       plain-widget-button-face
       plain-widget-button-pressed-face
       plain-widget-documentation-face
       plain-widget-field-face
       plain-widget-inactive-face
       plain-widget-single-line-field-face
       setnu-line-number-face
       show-paren-match-face
       show-paren-mismatch-face
       speedbar-button-face
       speedbar-directory-face
       speedbar-file-face
       speedbar-highlight-face
       speedbar-tag-face
       strokes-char-face
       todoo-item-assigned-header-face
       todoo-item-header-face
       todoo-sub-item-header-face
       tuareg-font-lock-governing-face
       tuareg-font-lock-interactive-error-face
       tuareg-font-lock-interactive-output-face
       tuareg-font-lock-operator-face
       w3m-anchor-face
       w3m-arrived-anchor-face
       w3m-form-button-face
       w3m-form-button-mouse-face
       w3m-form-button-pressed-face
       w3m-form-face
       w3m-image-face
       w3m-tab-background-face
       w3m-tab-selected-face
       w3m-tab-selected-retrieving-face
       w3m-tab-unselected-face
       w3m-tab-unselected-retrieving-face
       widget-button-face
       widget-button-highlight-face
       widget-button-pressed-face
       widget-button-pressed-highlight-face
       widget-documentation-face
       widget-field-face
       widget-inactive-face
       widget-single-line-field-face))
    )))

(defalias 'desertedocean #'color-theme-desertedocean)

(provide 'desertedocean)

;; Local Variables:
;; time-stamp-format: "%:y-%02m-%02d %02H:%02M"
;; time-stamp-start: "Updated: "
;; time-stamp-end: "$"
;; End:

;;; desertedocean.el ends here.
