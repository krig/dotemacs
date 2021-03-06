(eval-when-compile
  (require 'cl))

(defconst proctologist-description
  "A noctilux-based color theme based on a Light Table inspired color theme based on Solarized's definitions.")

(defcustom proctologist-degrade nil
  "For test purposes only; when in GUI mode, forces Proctologist to use the 256
degraded color mode to test the approximate color values for accuracy."
  :type 'boolean
  :group 'lt)

(defcustom proctologist-diff-mode 'normal
  "Sets the level of highlighting to use in diff-like modes."
  :type 'symbol
  :options '(high normal low)
  :group 'lt)

(defcustom proctologist-bold t
  "Stops Proctologist from displaying bold when nil."
  :type 'boolean
  :group 'lt)

(defcustom proctologist-underline t
  "Stops Proctologist from displaying underlines when nil."
  :type 'boolean
  :group 'lt)

(defcustom proctologist-italic t
  "Stops Proctologist from displaying italics when nil."
  :type 'boolean
  :group 'lt)

(defcustom proctologist-contrast 'normal
  "Stick with normal! It's been carefully tested. Setting this option to high or
low does use the same Proctologist palette but simply shifts some values up or
down in order to expand or compress the tonal range displayed."
  :type 'symbol
  :options '(high normal low)
  :group 'lt)

(defcustom proctologist-broken-srgb (and (eq system-type 'darwin)
                                     (eq window-system 'ns)
                                     (not (when (boundp 'ns-use-srgb-colorspace)
                                            ns-use-srgb-colorspace)))
  "Emacs bug #8402 results in incorrect color handling on Macs. If this is t
\(the default on Macs), Proctologist works around it with alternative colors.
However, these colors are not totally portable, so you may be able to edit
the \"Gen RGB\" column in proctologist-definitions.el to improve them further."
  :type 'boolean
  :group 'lt)

;; FIXME: The Generic RGB colors will actually vary from device to device, but
;;        hopefully these are closer to the intended colors than the sRGB values
;;        that Emacs seems to dislike
;; 2a2c2e
(defvar proctologist-colors           ; ANSI(Proctologist terminal)
  ;; name     sRGB      Gen RGB   256       16              8
  '((base03  "#23262e" "#23262e" "#23262e" "brightblack"   "black")
    (base02  "#1f2229" "#1f2229" "#1f2229" "black"         "black")
    (base01  "#45484f" "#45484f" "#45484f" "brightgreen"   "green")
    (base00  "#9f9f9f" "#9f9f9f" "#9f9f9f" "brightyellow"  "yellow")
    (base0   "#cfcfcf" "#cfcfcf" "#cfcfcf" "brightblue"    "blue")
    (base1   "#aaaaaa" "#aaaaaa" "#aaaaaa" "brightcyan"    "cyan")
    (base2   "#e9e2cb" "#e9e2cb" "#e9e2cb" "white"         "white")
    (base3   "#fcf4dc" "#fcf4dc" "#fcf4dc" "brightwhite"   "white")
    (yellow  "#fdd1a1" "#fdd1a1" "#fdd1a1" "yellow"        "yellow")
    (orange  "#ff8888" "#ff8888" "#ff8888" "brightred"     "red")
    (red     "#ff3333" "#ff3333" "#ff3333" "red"           "red")
    (magenta "#ff1f69" "#ff1f69" "#ff1f69" "magenta"       "magenta")
    (violet  "#afffd1" "#afffd1" "#afffd1" "brightmagenta" "magenta")
    (blue    "#cdffaf" "#cdffaf" "#cdffaf" "blue"          "blue")
    (cyan    "#e4e7a3" "#e4e7a3" "#e4e7a3" "cyan"          "cyan")
    (white   "#ffffff" "#ffffff" "#ffffff" "white"          "white")
    (green   "#ffb1af" "#ffb1af" "#ffb1af" "green"         "green"))
  "This is a table of all the colors used by the Proctologist color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

(defmacro proct-flet (specs &rest body)
  (let ((flet (if (fboundp 'cl-flet) 'cl-flet 'flet)))
    `(,flet ,specs ,@body)))

(defun proctologist-color-definitions (mode)
  (proct-flet ((find-color (name)
           (let* ((index (if window-system
                             (if proctologist-degrade
                                 3
                               (if proctologist-broken-srgb 2 1))
                           (case (display-color-cells)
                             (16 4)
                             (8  5)
                             (otherwise 3)))))
             (nth index (assoc name proctologist-colors)))))
    (let ((base03      (find-color 'base03))
          (base02      (find-color 'base02))
          (base01      (find-color 'base01))
          (base00      (find-color 'base00))
          (base0       (find-color 'base0))
          (base1       (find-color 'base1))
          (base2       (find-color 'base2))
          (base3       (find-color 'base3))
          (yellow      (find-color 'yellow))
          (orange      (find-color 'orange))
          (red         (find-color 'red))
          (magenta     (find-color 'magenta))
          (violet      (find-color 'violet))
          (blue        (find-color 'blue))
          (cyan        (find-color 'cyan))
          (white       (find-color 'white))
          (green       (find-color 'green))
          (bold        (if proctologist-bold 'bold 'normal))
          (bright-bold (if proctologist-bold 'normal 'bold))
          (underline   (if proctologist-underline t nil))
          (opt-under   nil)
          (italic      (if proctologist-italic 'italic 'normal)))
      (let ((back base03))
        (cond ((< (display-color-cells) 16)
               (setf back nil))
              ((eq 'high proctologist-contrast)
               (let ((orig-base3 base3))
                 (rotatef base01 base00 base0 base1 base2 base3)
                 (setf base3 orig-base3)))
              ((eq 'low proctologist-contrast)
               (setf back      base02
                     opt-under t)))
        ;; NOTE: We try to turn an 8-color term into a 10-color term by not
        ;;       using default background and foreground colors, expecting the
        ;;       user to have the right colors set for them.
        (let ((bg-back   `(:background ,back))
              (bg-base03 `(:background ,base03))
              (bg-base02 `(:background ,base02))
              (bg-base01 `(:background ,base01))
              (bg-base00 `(:background ,base00))
              (bg-base0 `(:background ,base0))
              (bg-base1 `(:background ,base1))
              (bg-base2 `(:background ,base2))
              (bg-base3 `(:background ,base3))
              (bg-green `(:background ,green))
              (bg-yellow `(:background ,yellow))
              (bg-orange `(:background ,orange))
              (bg-red `(:background ,red))
              (bg-magenta `(:background ,magenta))
              (bg-violet `(:background ,violet))
              (bg-blue `(:background ,blue))
              (bg-cyan `(:background ,cyan))
              (bg-white `(:background ,white))

              (fg-base03 `(:foreground ,base03))
              (fg-base02 `(:foreground ,base02))
              (fg-base01 `(:foreground ,base01))
              (fg-base00 `(:foreground ,base00))
              (fg-base0 `(:foreground ,(when (<= 16 (display-color-cells))
                                         base0)))
              (fg-base1 `(:foreground ,(when (<= 16 (display-color-cells))
                                         base1)))
              (fg-base2 `(:foreground ,base2))
              (fg-base3 `(:foreground ,base3))
              (fg-green `(:foreground ,green))
              (fg-yellow `(:foreground ,yellow))
              (fg-orange `(:foreground ,orange))
              (fg-red `(:foreground ,red))
              (fg-magenta `(:foreground ,magenta))
              (fg-violet `(:foreground ,violet))
              (fg-blue `(:foreground ,blue))
              (fg-cyan `(:foreground ,cyan))

              (paren-1 `(:foreground "#696969"))
              (paren-2 `(:foreground "#717171"))
              (paren-3 `(:foreground "#797979"))
              (paren-4 `(:foreground "#818181"))
              (paren-5 `(:foreground "#898989"))
              (paren-6 `(:foreground "#919191"))
              (paren-7 `(:foreground "#999999"))
              (paren-8 `(:foreground "#a1a1a1"))
              (paren-9 `(:foreground "#a9a9a9"))

              (fmt-none `(:weight normal :slant normal  :underline nil        :inverse-video nil))
              (fmt-bold `(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
              (fmt-bldi `(:weight ,bold                 :underline nil        :inverse-video nil))
              (fmt-undr `(:weight normal :slant normal  :underline ,underline :inverse-video nil))
              (fmt-undb `(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
              (fmt-undi `(:weight normal                :underline ,underline :inverse-video nil))
              (fmt-uopt `(:weight normal :slant normal  :underline ,opt-under :inverse-video nil))
              ;; FIXME: not quite the same
              (fmt-curl `(:weight normal :slant normal  :underline t          :inverse-video nil))
              (fmt-ital `(:weight normal :slant ,italic :underline nil        :inverse-video nil))
              ;; FIXME: not quite the same
              (fmt-stnd `(:weight normal :slant normal  :underline nil        :inverse-video t))
              (fmt-revr `(:weight normal :slant normal  :underline nil        :inverse-video t))
              (fmt-revb `(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
              (fmt-revbb `(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
              (fmt-revbbu `(:weight ,bright-bold :slant normal  :underline ,underline :inverse-video t))
              (fmt-redwave `(:underline (:color ,red :style wave) :inherit default))
              (fmt-orangewave `(:underline (:color ,orange :style wave) :inherit default))
              (fmt-bluewave `(:underline (:color ,blue :style wave) :inherit default)))
          `((;; basic
             (default ((t (,@fg-base0 ,@bg-back)))) ; Normal
             (cursor ((t (,@fg-base03 ,@bg-white)))) ; Cursor
             (error ((t (,@fmt-bold ,@fg-red)))) ; Error
             (escape-glyph-face ((t (,@fg-red))))
             (fringe ((t (,@fg-base01 ,@bg-base02))))
             (linum ((t (,@fg-base01 ,@bg-base02))))
             (header-line ((t (,@fg-base0 ,@bg-base02 ,@fmt-revbb)))) ; Pmenu
             (highlight ((t (,@bg-base02))))
             (hl-line ((t (:underline ,opt-under ,@bg-base02)))) ; CursorLine
             (isearch ((t (,@fmt-stnd ,@fg-orange ,@bg-back)))) ; IncSearch
             (isearch-fail ((t (,@fmt-stnd ,@fg-orange ,@bg-back)))) ; IncSearch
             (lazy-highlight ((t (,@fmt-revr ,@fg-yellow ,@bg-back)))) ; Search
             (link ((t (,@fmt-undr ,@fg-violet))))
             (link-visited ((t (,@fmt-undr ,@fg-magenta))))
             (menu ((t (,@fg-base0 ,@bg-base02))))
             (minibuffer-prompt ((t (,@fmt-bold ,@fg-cyan)))) ; Question
             (mode-line  ; StatusLine
              ((t (,@fg-base00 ,@bg-base02 ,@fmt-revbb :box nil))))
             (mode-line-inactive ; StatusLineNC
              ((t (,@fg-base01 ,@bg-base02 ,@fmt-revbb :box nil))))
             (region ((t (,@fg-base01 ,@bg-base03 ,@fmt-revbb)))) ; Visual
             (secondary-selection ((t (,@bg-base02))))
             (shadow ((t (,@fg-base00))))
             (trailing-whitespace ((t (,@fmt-revr ,@fg-red))))
             (vertical-border ((t (,@fg-base0))))
             ;; comint
             (comint-highlight-prompt ((t (,@fg-blue))))
             ;; compilation
             (compilation-info ((t (,@fmt-bold ,@fg-green))))
             (compilation-warning ((t (,@fmt-bold ,@fg-orange))))
             ;; custom
             (custom-button
              ((t (,@fg-base1 ,@bg-base02
                              :box (:line-width 2 :style released-button)))))
             (custom-button-mouse
              ((t (,@fmt-revr ,@fg-base1 ,@bg-base02 :inherit custom-button))))
             (custom-button-pressed
              ((t (,@fmt-revr ,@fg-base1 ,@bg-base02
                              :box (:line-width 2 :style pressed-button)
                              :inherit custom-button-mouse))))
             (custom-changed ((t (,@fmt-revr ,@fg-blue ,@bg-base3))))
             (custom-comment ((t (,@fg-base1 ,@bg-base2))))
             (custom-comment-tag ((t (,@fg-base1 ,@bg-base2))))
             (custom-documentation ((t (:inherit default))))
             (custom-group-tag ((t (,@fg-base1))))
             (custom-group-tag-1 ((t (,fmt-bold ,@fg-base1))))
             (custom-invalid ((t (,@fmt-revr ,@fg-red ,@bg-back))))
             (custom-link ((t (,@fg-violet))))
             (custom-state ((t (,@fg-green))))
             (custom-variable-tag ((t (,@fg-base1))))
             ;; diff - DiffAdd, DiffChange, DiffDelete, and DiffText
             ,@(case proctologist-diff-mode
                 (high
                  `((diff-added ((t (,@fmt-revr ,@fg-green))))
                    (diff-changed ((t (,@fmt-revr ,@fg-yellow))))
                    (diff-removed ((t (,@fmt-revr ,@fg-red))))
                    (diff-refine-change
                     ((t (,@fmt-revr ,@fg-blue ,@bg-back))))))
                 (low
                  `((diff-added ((t (,@fmt-undr ,@fg-green))))
                    (diff-changed ((t (,@fmt-undr ,@fg-yellow))))
                    (diff-removed ((t (,@fmt-bold ,@fg-red))))
                    (diff-refine-change
                     ((t (,@fmt-undr ,@fg-blue ,@bg-back))))))
                 (normal
                  (if window-system
                      `((diff-added ((t (,@fmt-bold ,@fg-green))))
                        (diff-changed ((t (,@fmt-bold ,@fg-yellow))))
                        (diff-removed ((t (,@fmt-bold ,@fg-red))))
                        (diff-refine-change
                         ((t (,@fmt-bold ,@fg-blue ,@bg-back)))))
                    `((diff-added ((t (,@fg-green))))
                      (diff-changed ((t (,@fg-yellow))))
                      (diff-removed ((t (,@fg-red))))
                      (diff-refine-change ((t (,@fg-blue ,@bg-back))))))))
             (diff-file-header ((t (,@bg-back))))
             (diff-header ((t (,@fg-base1 ,@bg-back))))
             ;; IDO
             (ido-only-match ((t (,@fg-green))))
             (ido-subdir ((t (,@fg-blue))))
             (ido-first-match ((t (,@fmt-bold ,@fg-green))))
             ;; emacs-wiki
             (emacs-wiki-bad-link-face ((t (,@fmt-undr ,@fg-red))))
             (emacs-wiki-link-face ((t (,@fmt-undr ,@fg-blue))))
             (emacs-wiki-verbatim-face ((t (,@fmt-undr ,@fg-base00))))
             ;; eshell
             (eshell-ls-archive ((t (,@fg-magenta))))
             (eshell-ls-backup ((t (,@fg-yellow))))
             (eshell-ls-clutter ((t (,@fg-orange))))
             (eshell-ls-directory ((t (,@fg-blue)))) ; Directory
             (eshell-ls-executable ((t (,@fg-green))))
             (eshell-ls-missing ((t (,@fg-red))))
             (eshell-ls-product ((t (,@fg-yellow))))
             (eshell-ls-readonly ((t (,@fg-base1))))
             (eshell-ls-special ((t (,@fg-violet))))
             (eshell-ls-symlink ((t (,@fg-cyan))))
             (eshell-ls-unreadable ((t (,@fg-base00))))
             (eshell-prompt ((t (,@fmt-bold ,@fg-green))))
             ;; font-lock
             (font-lock-builtin-face ((t (,@fmt-none ,@fg-green)))) ; Statement
             (font-lock-comment-face ((t (,@fmt-ital ,@fg-base2)))) ; Comment
             (font-lock-constant-face ((t (,@fmt-none ,@fg-violet)))) ; Constant
             (font-lock-function-name-face ; Identifier
              ((t (,@fmt-none ,@fg-blue))))
             (font-lock-keyword-face ((t (,@fmt-none ,@fg-green)))) ; Statement
             (font-lock-string-face ((t (,@fmt-none ,@fg-cyan)))) ; Constant
             (font-lock-type-face ((t (,@fmt-none ,@fg-yellow)))) ; Type
             (font-lock-variable-name-face ; Identifier
              ((t (,@fmt-none ,@fg-blue))))
             (font-lock-warning-face ((t (,@fmt-bold ,@fg-red)))) ; Error
             (font-lock-doc-face ((t (,@fmt-ital ,@fg-base2)))) ; Comment
             (font-lock-doc-string-face  ; Comment (XEmacs-only)
              ((t (,@fmt-ital ,@fg-base2))))
             (font-lock-color-constant-face ((t (,@fmt-none ,@fg-green))))
             (font-lock-comment-delimiter-face ; Comment
              ((t (,@fmt-ital ,@fg-base2))))
             (font-lock-preprocessor-face ; PreProc
              ((t (,@fmt-none ,@fg-orange))))
             (font-lock-reference-face ((t (,@fmt-none ,@fg-cyan))))
             (font-lock-negation-char-face ((t (,@fmt-none ,@fg-red))))
             (font-lock-other-type-face ((t (,@fmt-ital ,@fg-blue))))
             (font-lock-regexp-grouping-construct
              ((t (,@fmt-none ,@fg-orange))))
             (font-lock-special-keyword-face ; Special
              ((t (,@fmt-none ,@fg-red))))
             (font-lock-exit-face ((t (,@fmt-none ,@fg-red))))
             (font-lock-other-emphasized-face ((t (,@fmt-bldi ,@fg-violet))))
             (font-lock-regexp-grouping-backslash
              ((t (,@fmt-none ,@fg-yellow))))
             ;; info
             (info-xref ((t (,@fmt-undr ,@fg-blue))))
             (info-xref-visited ((t (,@fg-magenta :inherit info-xref))))
             ;; org
             (org-hide ((t (,@fg-base03))))
             (org-todo ((t (,@fmt-bold ,@fg-base03 ,@bg-red))))
             (org-done ((t (,@fmt-bold ,@fg-green))))
             (org-todo-kwd-face ((t (,@fg-red ,@bg-base03))))
             (org-done-kwd-face ((t (,@fg-green ,@bg-base03))))
             (org-project-kwd-face ((t (,@fg-violet ,@bg-base03))))
             (org-waiting-kwd-face ((t (,@fg-orange ,@bg-base03))))
             (org-someday-kwd-face ((t (,@fg-blue ,@bg-base03))))
             (org-started-kwd-face ((t (,@fg-yellow ,@bg-base03))))
             (org-cancelled-kwd-face ((t (,@fg-green ,@bg-base03))))
             (org-delegated-kwd-face ((t (,@fg-cyan ,@bg-base03))))
             ;; table
             (table-cell ((t (,@fmt-none ,@fg-base0 ,@bg-back))))
             ;; outline - pandocBlockQuoteLeader*
             (outline-1 ((t (,@fmt-none ,@fg-blue))))
             (outline-2 ((t (,@fmt-none ,@fg-cyan))))
             (outline-3 ((t (,@fmt-none ,@fg-yellow))))
             (outline-4 ((t (,@fmt-none ,@fg-red))))
             (outline-5 ((t (,@fmt-none ,@fg-base0))))
             (outline-6 ((t (,@fmt-none ,@fg-base01))))
             (outline-7 ((t (,@fmt-none ,@fg-orange))))
             (outline-8 ((t (,@fmt-none ,@fg-violet))))
             ;; speedbar
             (speedbar-button-face ((t (,@fmt-none ,@fg-base1))))
             (speedbar-directory-face ((t (,@fmt-none ,@fg-orange))))
             (speedbar-file-face ((t (,@fmt-none ,@fg-green))))
             (speedbar-highlight-face ((t (,@bg-base02))))
             (speedbar-selected-face ((t (,@fmt-undr ,@fg-yellow))))
             (speedbar-separator-face ((t (,@fmt-stnd))))
             (speedbar-tag-face ((t (,@fmt-none ,@fg-blue))))
             ;; show-paren - MatchParen
             (show-paren-match ((t (,@fmt-revr ,@fg-cyan ,@bg-base02))))
             (show-paren-mismatch ((t (,@fmt-revr ,@fg-red ,@bg-base01))))
             ;; widgets
             (widget-field
              ((t (,@fg-base1 ,@bg-base02 :box (:line-width 1)
                              :inherit default))))
             (widget-single-line-field ((t (:inherit widget-field))))
             ;; extra modules
             ;; -------------
	     ;; bm visual bookmarks
	     (bm-fringe-face ((t (,@bg-orange ,@fg-base03))))
	     (bm-fringe-persistent-face ((t (,@bg-blue ,@fg-base03))))
             ;; Flymake
             (flymake-errline ((t (,@fmt-redwave)))) ; ErrorMsg
             (flymake-warnline ((t (,@fmt-orangewave)))) ; WarningMsg
             (flycheck-error ((t (,@fmt-redwave)))) ; ErrorMsg
             (flycheck-warning ((t (,@fmt-orangewave)))) ; WarningMsg
             (flycheck-info ((t (,@fmt-bluewave))))
             (flycheck-fringe-error ((t (,@fg-red)))) ; ErrorMsg
             (flycheck-fringe-warning ((t (,@fg-orange)))) ; WarningMsg
             (flycheck-fringe-info ((t (,@fg-blue))))
             ;; column-marker
             (column-marker-1 ((t (,@bg-base01))))
             (column-marker-2 ((t (,@bg-cyan))))
             (column-marker-3 ((t (,@bg-violet))))
             ;; jabber
             (jabber-activity-face ((t (,@fmt-bold ,@fg-red))))
             (jabber-activity-personal-face ((t (,@fmt-bold ,@fg-blue))))
             (jabber-chat-error ((t (,@fmt-bold ,@fg-red))))
             (jabber-chat-prompt-foreign ((t (,@fmt-bold ,@fg-red))))
             (jabber-chat-prompt-local ((t (,@fmt-bold ,@fg-blue))))
             (jabber-chat-prompt-system ((t (,@fmt-bold ,@fg-green))))
             (jabber-chat-text-foreign ((t (,@fg-base1))))
             (jabber-chat-text-local ((t (,@fg-base0))))
             (jabber-chat-rare-time-face ((t (,@fmt-undr ,@fg-green))))
             (jabber-roster-user-away ((t (,@fmt-ital ,@fg-green))))
             (jabber-roster-user-chatty ((t (,@fmt-bold ,@fg-orange))))
             (jabber-roster-user-dnd ((t (,@fmt-ital ,@fg-red))))
             (jabber-roster-user-error ((t (:weight light :slant italic ,@fg-red))))
             (jabber-roster-user-offline ((t (,@fg-base01))))
             (jabber-roster-user-online ((t (,@fmt-bold ,@fg-blue))))
             (jabber-roster-user-xa ((t (,@fmt-ital ,@fg-magenta))))
	     ;; git-gutter
	     (git-gutter:modified ((t (,@fg-violet))))
	     (git-gutter:added ((t (,@fg-green))))
	     (git-gutter:deleted ((t (,@fg-red))))
             ;; gnus - these are taken from mutt, not VIM
             (gnus-cite-1 ((t (,@fmt-none ,@fg-blue)))) ; quoted
             (gnus-cite-2 ((t (,@fmt-none ,@fg-cyan)))) ; quoted1
             (gnus-cite-3 ((t (,@fmt-none ,@fg-yellow)))) ; quoted2
             (gnus-cite-4 ((t (,@fmt-none ,@fg-red)))) ; quoted3
             (gnus-cite-5 ((t (,@fmt-none ,@fg-orange)))) ; quoted4
             (gnus-cite-6 ((t (,@fmt-none ,@fg-violet))))
             (gnus-cite-7 ((t (,@fmt-none ,@fg-green))))
             (gnus-cite-8 ((t (,@fmt-none ,@fg-magenta))))
             (gnus-cite-9 ((t (,@fmt-none ,@fg-base00))))
             (gnus-cite-10 ((t (,@fmt-none ,@fg-base01))))
             (gnus-cite-11 ((t (,@fmt-none ,@fg-base02))))
             (gnus-group-mail-1 ((t (,@fmt-bold ,@fg-base3))))
             (gnus-group-mail-1-empty ((t (,@fg-base3))))
             (gnus-group-mail-2 ((t (,@fmt-bold ,@fg-base2))))
             (gnus-group-mail-2-empty ((t (,@fg-base2))))
             (gnus-group-mail-3 ((t (,@fmt-bold ,@fg-magenta))))
             (gnus-group-mail-3-empty ((t (,@fg-magenta))))
             (gnus-group-mail-low ((t (,@fmt-bold ,@fg-base00))))
             (gnus-group-mail-low-empty ((t (,@fg-base00))))
             (gnus-group-news-1 ((t (,@fmt-bold ,@fg-base1))))
             (gnus-group-news-1-empty ((t (,@fg-base1))))
             (gnus-group-news-2 ((t (,@fmt-bold ,@fg-blue))))
             (gnus-group-news-2-empty ((t (,@fg-blue))))
             (gnus-group-news-low ((t (,@fmt-bold ,@fg-violet))))
             (gnus-group-news-low-empty ((t (,@fg-violet))))
             (gnus-emphasis-highlight-words ; highlight
              ((t (,@fmt-none ,fg-yellow))))
             (gnus-header-content ((t (,@fmt-none ,@fg-base01)))) ; hdrdefault
             (gnus-header-from ((t (,@fmt-none ,@fg-base00)))) ; header ^From
             (gnus-header-name ((t (,@fmt-none ,@fg-base01)))) ; hdrdefault
             (gnus-header-newsgroups ; hdrdefault
              ((t (,@fmt-none ,@fg-base02))))
             (gnus-header-subject ; header ^Subject
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-server-agent ((t (,@fmt-bold ,@fg-base3))))
             (gnus-server-closed ((t (,@fmt-ital ,@fg-base1))))
             (gnus-server-denied ((t (,@fmt-bold ,@fg-base2))))
             (gnus-server-offline ((t (,@fmt-bold ,@fg-green))))
             (gnus-server-opened ((t (,@fmt-bold ,@fg-cyan))))
             (gnus-signature ((t (,@fmt-none ,@fg-base01)))) ; signature
             (gnus-splash ((t (,@fg-base2))))
             (gnus-summary-cancelled ; deleted messages
              ((t (,@fmt-none ,@fg-red))))
             (gnus-summary-high-ancient
              ((t (,@fmt-bold :inherit gnus-summary-normal-ancient))))
             (gnus-summary-high-read
              ((t (,@fmt-bold :inherit gnus-summary-normal-read))))
             (gnus-summary-high-ticked
              ((t (,@fmt-bold :inherit gnus-summary-normal-ticked))))
             (gnus-summary-high-undownloaded
              ((t (,@fmt-bold :inherit gnus-summary-normal-undownloaded))))
             (gnus-summary-high-unread
              ((t (,@fmt-bold :inherit gnus-summary-normal-unread))))
             (gnus-summary-low-ancient
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-read
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-unread
              ((t (,@fmt-ital :inherit gnus-summary-normal-unread))))
             (gnus-summary-low-ticked
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-undownloaded
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-normal-ancient ; old messages
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-summary-normal-read ; read messages
              ((t (,@fmt-none ,@fg-base01))))
             (gnus-summary-normal-ticked ; flagged
              ((t (,@fmt-none ,@fg-red))))
             (gnus-summary-normal-undownloaded ((t (,@fmt-none ,@fg-base2))))
             (gnus-summary-normal-unread ; unread messages
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-summary-selected ; indicator
              ((t (,@fmt-none ,@fg-base03 ,@bg-yellow))))
             ;; Message
             (message-mml ((t (,@fg-blue))))
             (message-cited-text ((t (,@fg-base2))))
             (message-separator ((t (,@fg-base3))))
             (message-header-xheader ((t (,@fg-violet))))
             (message-header-name ((t (,@fg-cyan))))
             (message-header-other ((t (,@fg-red))))
             (message-header-newsgroups ((t (,@fmt-bldi ,@fg-yellow))))
             (message-header-subject ((t (,@fg-base00))))
             (message-header-cc ((t (,@fmt-bold ,@fg-green))))
             (message-header-to ((t (,@fmt-bold ,@fg-base1))))
             ;; parenface
             (paren-face ((t (,@fg-base01))))
             ;; rainbow-delimiters
             (rainbow-delimiters-depth-1-face ((t (,@paren-1))))
             (rainbow-delimiters-depth-2-face ((t (,@paren-2))))
             (rainbow-delimiters-depth-3-face ((t (,@paren-3))))
             (rainbow-delimiters-depth-4-face ((t (,@paren-4))))
             (rainbow-delimiters-depth-5-face ((t (,@paren-5))))
             (rainbow-delimiters-depth-6-face ((t (,@paren-6))))
             (rainbow-delimiters-depth-7-face ((t (,@paren-7))))
             (rainbow-delimiters-depth-8-face ((t (,@paren-8))))
             (rainbow-delimiters-depth-9-face ((t (,@paren-9))))
             ;; slime
             (slime-error-face ((t (,@fmt-revr ,@fg-red)))) ; ErrorMsg
             (slime-note-face ((t (,@fg-yellow))))
             (slime-repl-inputted-output-face ((t (,@fg-red))))
             (slime-repl-output-mouseover-face ((t (:box (:color ,base3)))))
             (slime-style-warning-face ((t (,@fmt-bold ,@fg-orange))))
             (slime-warning-face ((t (,@fmt-bold ,@fg-red)))) ; WarningMsg
             ;; whitespace
             (whitespace-empty ((t (,@fg-red))))
             (whitespace-hspace ((t (,@fg-orange))))
             (whitespace-indentation ((t (,@fg-green))))
             (whitespace-space ((t (,@fg-green))))
             (whitespace-space-after-tab ((t (,@fg-cyan))))
             (whitespace-space-before-tab ((t (,@fmt-bold ,@fg-red))))
             (whitespace-tab ((t (,@fg-green))))
             (whitespace-trailing ((t (,@fmt-bold ,@fg-red ,@bg-base01))))
             (whitespace-highlight-face ((t (,@fg-red ,@bg-blue))))
             (whitespace-line ((t (,@fg-magenta ,@bg-base03))))
             ;; rcirc
             (rcirc-my-nick ((t (:foreground ,blue))))
             (rcirc-nick-in-message ((t (:foreground ,orange))))
             (rcirc-other-nick ((t (:foreground ,green))))
             (rcirc-prompt ((t (:foreground ,yellow))))
             (rcirc-bright-nick ((t (:foreground ,magenta))))
             (rcirc-server ((t (:foreground ,base1))))
             (rcirc-timestamp ((t (:foreground ,base01))))
             ;; ERC
             (erc-input-face ((t (:foreground ,base01))))
             (erc-keyword-face ((t (,@fmt-bldi ,@fg-yellow))))
             (erc-my-nick-face ((t (:foreground ,blue))))
             (erc-nick-defauproctologist-face ((t (,@fmt-none ,@fg-cyan))))
             (erc-notice-face ((t (,@fmt-none ,@fg-blue))))
             (erc-timestamp-face ((t (:foreground ,base01))))
             ;;font-latex
             (font-latex-warning-face ((t (,@fg-red))))
             (font-latex-sectioning-5-face ((t (,@fg-violet))))
             ;;flyspell
             (flyspell-incorrect ((t (,@fg-red))))
             (flyspell-duplicate ((t (,@fg-yellow))))
             ;; company-mode
             (company-tooltip ((t (,@fg-base0 ,@bg-base02))))
             (company-tooltip-selection ((t (,@fg-base0 ,@bg-base01))))
             (company-tooltip-mouse ((t (,@bg-base02))))
             (company-tooltip-common ((t (,@bg-base02 ,@fg-violet))))
             (company-tooltip-common-selection ((t (,@fg-violet ,@bg-base01))))
             (company-tooltip-annotation ((t (,@fg-base0 ,@bg-base02))))
             (company-scrollbar-fg ((t (,@bg-base01))))
             (company-scrollbar-bg ((t (,@bg-base3))))
             (company-preview ((t (,@fg-base0 ,@bg-base01))))
             (company-preview-common ((t (,@fg-base0 ,@bg-base01))))
             (company-preview-search ((t (,@fg-violet ,@bg-base01))))
             (company-echo ((t nil)))
             (company-echo-common ((t (,@fg-violet))))
	     ;;ansi-term
	     (term-color-black ((t ( ,@fg-base02))))
	     (term-color-red ((t ( ,@fg-red))))
	     (term-color-green ((t ( ,@fg-green))))
	     (term-color-yellow ((t ( ,@fg-yellow))))
	     (term-color-blue ((t ( ,@fg-blue))))
	     (term-color-magenta ((t ( ,@fg-magenta))))
	     (term-color-cyan ((t ( ,@fg-cyan))))
	     (term-color-white ((t ( ,@fg-base00)))))

            ((foreground-color . ,(when (<= 16 (display-color-cells)) base0))
             (background-color . ,back)
             (background-mode . ,mode)
             (cursor-color . ,(when (<= 16 (display-color-cells))
                                base0))
	     (ansi-color-names-vector . [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00]))))))))

(defmacro create-proctologist-theme ()
  (let* ((theme-name 'proctologist)
         (defs (proctologist-color-definitions 'dark))
         (theme-vars (mapcar (lambda (def) (list (car def) (cdr def)))
                             (second defs)))
         (theme-faces (first defs)))
    `(progn
       (deftheme ,theme-name ,proctologist-description)
       (apply 'custom-theme-set-variables ',theme-name ',theme-vars)
       (apply 'custom-theme-set-faces ',theme-name ',theme-faces)
       (provide-theme ',theme-name))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'proctologist-definitions)
