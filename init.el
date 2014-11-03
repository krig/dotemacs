;; emacs config
;; TODO:
;; cleanup, modularize...
;;

(setq frame-title-format "%b %*")

(defun krig-macp ()
  (string-match "apple-darwin" system-configuration))

(defun krig-linuxp ()
  (string-match "linux" system-configuration))

(defun krig-winp ()
  (eq system-type 'windows-nt))

(setq hostname (getenv "HOSTNAME"))

(pcase hostname
  ("krigpad.site" (setq user-full-name "Kristoffer Grönlund"
                        user-mail-address (concat "kgronlund@" "suse" ".com")))
  (_ (setq user-full-name "Kristoffer Grönlund"
           user-mail-address (concat "krig@" "koru" ".se"))))

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
     ("kowloon" "Ubuntu Mono-17")
     ("krigpad.site" "Ubuntu Mono-14")
     (_ "Ubuntu Mono-13")))
  (menu-bar-mode -1))

(setq custom-safe-themes '("2233263f8185428aa9c6df1d32353cff86f09ec8a008983c9f799f4efc341b31" "bb27775d3f6e75ea0faa855ecf3eea6744e0951378474f9a3e29908f3fdfb3cd" "36afe64261e1de73fcfadedf154e4bc2c9ec1969bde0c21798d31366897bc4d2" default))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/flatui-theme")

(setq custom-file "~/.emacs.d/custom.el")

(load custom-file 'noerror)

;; emacs configuration
(autoload 'magit-status "magit" nil t)

(push "/usr/local/bin" exec-path)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/smart-tab")
(add-to-list 'load-path "~/.emacs.d/smarttabs")
(add-to-list 'load-path "~/.emacs.d/haml-mode")

(setq comment-style 'indent)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "linux")
;;(setq-default c-basic-offset 4)
(setq inhibit-startup-message t)
(setq visible-bell t)
(auto-compression-mode 1)

;; hide warnings
(setq warning-minimum-level :error)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
;;(setq delete-active-region nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)
(line-number-mode t)
(column-number-mode -1)
(set-fringe-style -1)
(tooltip-mode -1)
(winner-mode 1)
(global-auto-revert-mode t)
(setq calendar-week-start-day 1)
(setq mark-even-if-inactive nil)

;; put something different in the scratch buffer
(setq initial-scratch-message ";)\n")

;; use ibuffer instead of regular buffer list
(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)



(setq compilation-skip-threshold 2)

(setq-default ispell-program-name "aspell")

;; C SOURCE FOR EMACS
(if (krig-winp)
    (setq find-function-C-source-directory "C:/Program/Emacs/src/src")
  (if (string-match "ultralix" hostname)
      (setq find-function-C-source-directory "~/src/extern/emacs-24.3/src")
    (setq find-function-C-source-directory "~/Personal/Sources/emacs/src")))

;; TRAMP
(require 'tramp)
(setq tramp-completion-reread-directory-timeout nil)
(setq tramp-default-method "scp")
;;(add-to-list 'tramp-remote-path "~/bin")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; ESHELL
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; ELPA
(require 'package)
(setq package-archives '(;("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(require 'el-get)

(setq el-get-sources
      '((:name ruby-mode
               :type elpa
               :load "ruby-mode.el"
               :after (progn (ruby-mode-hook)))
        (:name inf-ruby  :type elpa)
        (:name ruby-compilation :type elpa)
        (:name css-mode
               :type elpa
               :load "css-mode.el"
               :after (progn (css-mode-hook)))
        (:name sass-mode
               :type elpa
               :load "sass-mode.el"
               :after (progn (sass-mode-hook)))
        (:name textmate
               :type git
               :url "https://github.com/defunkt/textmate.el"
               :load "textmate.el")
        (:name rhtml
               :type git
               :url "https://github.com/eschulte/rhtml"
               :features rhtml-mode
               :after (progn (rhtml-mode-hook)))
        (:name yaml-mode
               :type git
               :url "https://github.com/yoshiki/yaml-mode"
               :features yaml-mode
               :after (progn (yaml-mode-hook)))))

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))
(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (rinari-launch)
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-hook ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

(defun sass-mode-hook ()
  (autoload 'sass-mode "css-mode" nil t))

(package-initialize)


;; NREPL / CLOJURE

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)



;; SCSS
(setq scss-compile-at-save nil)

(defun is-rails-project ()
  (when (textmate-project-root)
    (file-exists-p (expand-file-name "config/environment.rb" (textmate-project-root)))))

(defun run-rails-test-or-ruby-buffer ()
  (interactive)
  (if (is-rails-project)
      (let* ((path (buffer-file-name))
             (filename (file-name-nondirectory path))
             (test-path (expand-file-name "test" (textmate-project-root)))
             (command (list ruby-compilation-executable "-I" test-path path)))
        (pop-to-buffer (ruby-compilation-do filename command)))
    (ruby-compilation-this-buffer)))

(message "krig: %s" "(el-get 'sync)")

;; SYNC EL-GET
(el-get)

;; Packages I use:
;; C-h v package-activated-list
;; install the missing packages
(defun krig-setup-packages ()
  (interactive)
  (el-get 'sync)
  (message "krig: %s" "(el-get)")
  (el-get)

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (package '(auto-yasnippet cm-mode cmake-mode ctags ctags-update elpy company find-file-in-project flymake-python-pyflakes flymake-easy git-messenger github-browse-file highlight-indentation idomenu ipython jinja2-mode js2-mode magit git-rebase-mode git-commit-mode popup pretty-symbols pymacs python-pep8 python-pylint pyvenv rinari jump inflections findr ruby-compilation inf-ruby ruby-mode rust-mode spacegray-theme sublime-themes sublimity yasnippet))
    (unless (package-installed-p package)
      (package-install package)))

  (mapcar 'package-install
          '("git-commit-mode" "textmate" "smart-operator" "rainbow-delimiters" "lua-mode" "python-mode" "pymacs" "python-pep8" "python-pylint" "js2-mode" "elpy" "clojure-mode" "markdown-mode" "adoc-mode"))
  (package-install "flymake-python-pyflakes"))

(unless (boundp 'textmate-mode)
  (krig-setup-packages))

;; TEXTMATE
(textmate-mode)

;; ASCIIDOC
(when (boundp 'doc-mode)
  (add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
  (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode)))

;; RAINBOW-DELIMITERS

(defun krig-paren-clr (n)
  (let ((c (+ ?\x59 (* (1- n) 8))))
    (format "#%X%X%X" c c c)))

(defun krig-rainbow-face-n (n)
  (intern (format "rainbow-delimiters-depth-%d-face" n)))

(progn
  (require 'rainbow-delimiters)
  (global-rainbow-delimiters-mode)
  (cl-loop for i from 1 to 9 do
           (set-face-foreground (krig-rainbow-face-n i)
                                (krig-paren-clr i))))

;; PAREDIT
;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

;; PRETTY-SYMBOLS-MODE
(when (boundp 'pretty-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
  (add-hook 'lisp-mode-hook 'pretty-symbols-mode)
  (add-hook 'lisp-interaction-mode-hook 'pretty-symbols-mode)
  (add-hook 'scheme-mode-hook 'pretty-symbols-mode))


;; IDO
(ido-mode t)
(ido-everywhere t)
(setq ido-save-directory-list-file "~/.emacs.d/ido.history")
(setq ido-max-work-file-list 40)
(setq ido-enable-flex-matching t)
(setq confirm-nonexistent-file-or-buffer nil)
(add-hook 'ido-define-mode-map-hook 'ido-my-keys)

(defun ido-my-keys ()
  (define-key ido-mode-map "\t" 'ido-complete)
  (define-key ido-mode-map "\C-t" 'ido-toggle-regexp) ; same as in isearch
  (define-key ido-mode-map "\C-d" 'ido-enter-dired)) ; cool
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)

(defun aw-ido-completing-read-with-default (prompt entries predicate)
  (let* ((maybedft (find-tag-default))
         (compl (all-completions "" entries predicate))
         (dft (assoc-string maybedft compl)))
    (ido-completing-read
     prompt
     compl
     nil
     t
     nil
     nil
     dft)))

(defun aw-ido-find-tag ()
  (interactive)
  (find-tag (aw-ido-completing-read-with-default "Tag: " (tags-lazy-completion-table) nil)))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; Search for Symbol at Point
;; http://blog.jorgenschaefer.de/2012/11/emacs-search-for-symbol-at-point.html
(define-key isearch-mode-map (kbd "C-d")
  'fc/isearch-yank-symbol)
(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch, but it only looks for
the rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (when (and (not isearch-forward)
                isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))

;; SMART TAB
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))

(defun exit-emacs ()
  (interactive)
  (save-buffers-kill-emacs))
;(global-set-key (kbd "s-Q") 'save-buffers-kill-emacs)

;; UNIQUIFY - better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; RECENT FILES
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 40)

;; OPEN FOR DIRED
(require 'dired-x)
(defun dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

(when (string-match "apple-darwin" system-configuration)
  (define-key dired-mode-map "o" 'dired-open-mac)
  (setq ls-lisp-use-insert-directory-program t)      ;; use external ls
  (setq insert-directory-program "/usr/local/bin/gls"))

;;  (global-set-key [(hyper a)] 'mark-whole-buffer)
;;  (global-set-key [(hyper v)] 'yank)
;;  (global-set-key [(hyper c)] 'kill-ring-save)
;;  (global-set-key [(hyper s)] 'save-buffer)
;;  (global-set-key [(hyper l)] 'goto-line)
;;  (global-set-key [(hyper z)] 'undo)
;;  (global-set-key (kbd "H-q") 'save-buffers-kill-emacs)
;;  (setq mac-option-modifier 'meta)
;;  (setq mac-command-modifier 'hyper))

;; PASTE.SE
(defun paste-se-encode-uri-component (str)
  (mapconcat
   (lambda (x)
     (if (or (eq x 60) (eq x 62) (eq x 96)
             (and (>= x 34) (<= x 38)) (and (>= x 123) (<= x 255))
             (and (>= x 91) (<= x 94)) (and (>= x 0) (<= x 9))
             (and (>= x 11) (<= x 32)))
         (format "%%%x" x)
       (char-to-string x)))
   str ""))

(defun paste-se-query-string (pairs)
  (mapconcat
   (lambda (x) (format "%s=%s"
                       (paste-se-encode-uri-component (car x))
                       (paste-se-encode-uri-component (cdr x))))
   pairs "&"))

(defun paste-se-paste-region (beg end)
  (interactive "r")
  (paste-se-paste-string (buffer-substring beg end))
  (if transient-mark-mode
      (setq deactivate-mark t))
  nil)

(defun paste-se-paste-string (paste-string)
  (let ((url-request-method "POST")
        (url-request-data (paste-se-query-string
                           (list (cons "paste" paste-string)
                                 (cons "lang" "text")
                                 (cons "user" "")
                                 (cons "desc" ""))))
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (paste-cb (lambda (&rest args)
                    (set buffer (car (cdr args)))
                    (insert (plist-get (car args) :redirect)))))
    (let ((oldbuf (current-buffer)))
      (url-retrieve "http://paste.se/index.py" 'paste-cb (list oldbuf)))))

;; UNTABIFY
(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  "Tabify current buffer"
  (interactive)
  (tabify (point-min) (point-max)))


(require 'generic-x)

;; CREATE SCRATCH BUFFER

(defun create-scratch-buffer ()
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (format "*scratch%s*" (if (= n 0) "" n))
                   n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

;; LORD PROGRAMMATON
(define-generic-mode 'programmaton-mode
  '("#")
  '("def" "if" "case" "elif" "else" "for" "in" "unless" "while" "do" "end" "fn" "throw" "int" "int64" "string" "real" "type")
  '(("[0-9]+" . 'font-lock-variable-name-face)
    ("[\(\)]" . 'paren-face)
    ("\\\"[^\\\"]*\\\"" . 'font-lock-string-face)
    ("['][^']*[']" . 'font-lock-string-face))
  '("\\.lp$")
  '(smart-tab-mode)
  "A mode for lord programmaton files")

;; crmsh
(define-generic-mode 'crmsh-mode
  '("#")
  '("node" "primitive" "group" "clone" "master" "ms"
    "location" "colocation" "order" "rsc_ticket" "rsc_template"
    "property" "rsc_defaults" "op_defaults" "user" "role"
    "fencing_topology" "tag")
  '(("[A-Za-z_][A-Za-z0-9_-]+" . 'font-lock-variable-name-face)
    ("[0-9]+" . 'font-lock-constant-face)
    ("[\(\)]" . 'paren-face)
    ("\\\"[^\\\"]*\\\"" . 'font-lock-string-face)
    ("['][^']*[']" . 'font-lock-string-face))
  '("\\.crm$")
  '(smart-tab-mode)
  "A mode for crmsh")


;; HEADER SWITCH
(defun switch-cc-to-h ()
  "Switch fromm *.<impl> to *.<head> and vice versa."
  (interactive)
  (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name))
          (suffix (match-string 2 buffer-file-name)))
      (cond ((string-match suffix "c\\|cc\\|C\\|cpp\\|m")
             (cond ((file-exists-p (concat name ".h"))
                    (find-file (concat name ".h")))
                   ((file-exists-p (concat name ".hh"))
                    (find-file (concat name ".hh")))
                   ((file-exists-p (concat name ".hpp"))
                    (find-file (concat name ".hpp")))))
            ((string-match suffix "h\\|hh\\|hpp")
             (cond ((file-exists-p (concat name ".cc"))
                    (find-file (concat name ".cc")))
                   ((file-exists-p (concat name ".C"))
                    (find-file (concat name ".C")))
                   ((file-exists-p (concat name ".cpp"))
                    (find-file (concat name ".cpp")))
                   ((file-exists-p (concat name ".c"))
                    (find-file (concat name ".c")))
                   ((file-exists-p (concat name ".m")))))))))
(global-set-key [C-s-up] 'switch-cc-to-h)

(global-set-key (kbd "M-[") 'beginning-of-defun)
(global-set-key (kbd "M-]") 'end-of-defun)

(global-set-key (kbd "M-j") 'join-line)


;; LINUM MODE
;;(global-linum-mode)

;; REINDENT BUFFER
(defun reindent-buffer ()
  "Reindent the contents of the entire buffer."
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end)))

;; KILL-AND-JOIN-FORWARD
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
    Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; OPEN INIT.EL
(defun dot-emacs ()
  "Opens the .emacs file for editing."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

;; BROWSE KILL RING
(require 'browse-kill-ring+)
(browse-kill-ring-default-keybindings)

(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))
(ad-activate 'yank-pop)

;; SERVER
(when (not (eq system-type 'windows-nt))
  (server-start))

;;(load "~/.emacs.d/nxhtml/autostart.el")

;;(setq tab-width 4)



;; 'javascript 'python

(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")


(smart-tabs-insinuate 'c 'c++ 'java)

;; C/C++
;; (add-hook 'c-mode-hook 'smart-tabs-mode-enable)

;; JavaScript
;; (add-hook 'js2-mode-hook 'smart-tabs-mode-enable)
;;(smart-tabs-advice js2-indent-line js2-basic-offset)

;; Perl (cperl-mode)
;; (add-hook 'cperl-mode-hook 'smart-tabs-mode-enable)
;;(smart-tabs-advice cperl-indent-line cperl-indent-level)

;; Python
;;(add-hook 'python-mode-hook 'smart-tabs-mode-enable)
;;(smart-tabs-advice python-indent-line-1 python-indent)

;; Ruby
;;(add-hook 'ruby-mode-hook 'smart-tabs-mode-enable)
;;(smart-tabs-advice ruby-indent-line ruby-indent-level)


;; FIXME: IN COMMENTS
(require 'fic-mode)

;; Whitespace fixes
(setq whitespace-line-column 130)

;; MODE HOOKS

(defun krig-sh-mode-hook ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (turn-on-fic-mode)
  (setq sh-indent-comment t)
  (local-set-key [return] 'newline-and-indent))
;;  (whitespace-mode))

;;;; SH-MODE HACK
;; hack to fix sh-mode heredoc
(defun sh--inside-noncommand-expression (pos)
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (when (nth 1 ppss)
        (goto-char (nth 1 ppss))
        (or
         (pcase (char-after)
           ;; ((...)) or $((...)) or $[...] or ${...}. Nested
           ;; parenthesis can occur inside the first of these forms, so
           ;; parse backward recursively.
           (`?\( (eq ?\( (char-before)))
           ((or `?\{ `?\[) (eq ?\$ (char-before))))
         (sh--inside-noncommand-expression (1- (point))))))))

(defun sh-font-lock-open-heredoc (start string eol)
  "Determine the syntax of the \\n after a <<EOF.
START is the position of <<.
STRING is the actual word used as delimiter (e.g. \"EOF\").
INDENTED is non-nil if the here document's content (and the EOF mark) can
be indented (i.e. a <<- was used rather than just <<).
Point is at the beginning of the next line."
  (unless (or (memq (char-before start) '(?< ?>))
	      (sh-in-comment-or-string start)
              (sh--inside-noncommand-expression start))
    ;; We're looking at <<STRING, so we add "^STRING$" to the syntactic
    ;; font-lock keywords to detect the end of this here document.
    (let ((str (replace-regexp-in-string "['\"]" "" string))
          (ppss (save-excursion (syntax-ppss eol))))
      (if (nth 4 ppss)
          ;; The \n not only starts the heredoc but also closes a comment.
          ;; Let's close the comment just before the \n.
          (put-text-property (1- eol) eol 'syntax-table '(12))) ;">"
      (if (or (nth 5 ppss) (> (count-lines start eol) 1))
          ;; If the sh-escaped-line-re part of sh-here-doc-open-re has matched
          ;; several lines, make sure we refontify them together.
          ;; Furthermore, if (nth 5 ppss) is non-nil (i.e. the \n is
          ;; escaped), it means the right \n is actually further down.
          ;; Don't bother fixing it now, but place a multiline property so
          ;; that when jit-lock-context-* refontifies the rest of the
          ;; buffer, it also refontifies the current line with it.
          (put-text-property start (1+ eol) 'syntax-multiline t))
      (put-text-property eol (1+ eol) 'sh-here-doc-marker str)
      (prog1 sh-here-doc-syntax
        (goto-char (+ 2 start))))))

;; END SH-MODE HACK

(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char-untabify))))))

(defun backward-delete-char-hungry (arg &optional killp)
  "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and
    `backward-delete-char-untabify-method' for details."
  (interactive "*p\nP")
  (let ((backward-delete-char-untabify-method 'hungry))
    (backward-delete-char-untabify arg killp)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun kernel-style-mode-hook ()
  "Add kernel style"
  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
              (arglist-cont-nonempty
               c-lineup-gcc-asm-reg
               c-lineup-arglist-tabs-only)))))

(add-hook 'c-mode-common-hook 'kernel-style-mode-hook)

;; TODO:
;; better support for different modes for different trees

(defun krig-cc-mode-hook ()
  (setq tab-width 8)
  (setq c-basic-offset 8)
  (setq indent-tabs-mode t)
  (c-set-style "linux")
  ;;(smart-tabs-mode-enable)
  ;;(smart-tabs-advice c-indent-line c-basic-offset)
  ;;(smart-tabs-advice c-indent-region c-basic-offset)
  (subword-mode 1)
  (setq show-trailing-whitespace t)
  (turn-on-fic-mode)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent))

(defun krig-mode-hook ()
  (setq show-trailing-whitespace t)
  (turn-on-fic-mode)
  (subword-mode 1)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent))

(dolist (hook '(
                emacs-lisp-mode-hook
                lisp-mode-hook
                clojure-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                perl-mode-hook
                vala-mode-hook
                ruby-mode-hook
                csharp-mode-hook
                java-mode-hook
                objc-mode-hook
                ))
  (add-hook hook 'krig-mode-hook))

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'krig-cc-mode-hook))

(add-hook 'sh-mode-hook 'krig-sh-mode-hook)


(add-hook 'align-load-hook (lambda ()
       (add-to-list 'align-rules-list
                    '(text-column-whitespace
                      (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                      (group   . 2)
                      (modes   . align-text-modes)
                      (repeat  . t)))))

;; RUST MODE

(require 'rust-mode)


;; FIX ENUM CLASS IN C++11 (broken)

;; (defun inside-class-enum-p (pos)
;;   "Checks if POS is within the braces of a C++ \"enum class\"."
;;   (ignore-errors
;;     (save-excursion
;;       (goto-char pos)
;;       (up-list -1)
;;       (backward-sexp 1)
;;       (looking-back "enum[ \t]+class[ \t]+"))))

;; (defun align-enum-class (langelem)
;;   (if (inside-class-enum-p (c-langelem-pos langelem))
;;       0
;;     (c-lineup-topmost-intro-cont langelem)))

;; (defun align-enum-class-closing-brace (langelem)
;;   (if (inside-class-enum-p (c-langelem-pos langelem))
;;       '-
;;     '+))

;; (defun fix-enum-class ()
;;   "Setup `c++-mode' to better handle \"class enum\"."
;;   (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
;;   (add-to-list 'c-offsets-alist
;;                '(statement-cont . align-enum-class-closing-brace)))

;;(add-hook 'c++-mode-hook 'fix-enum-class)


(add-hook
 'java-mode-hook
 '(lambda () "Treat Java 1.5 @-style annotations as comments."
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))


(add-to-list 'auto-mode-alist '("\\.fsh$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vsh$" . c-mode))

;; '(js2-basic-offset 4)
;; '(js2-bounce-indent-flag nil)
;; '(js2-indent-on-enter-key t)

;; LUA MODE
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; VALA MODE
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; CSHARP MODE
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; MARKDOWN MODE
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(autoload 'markdown-mode "markdown-mode" "Markdown editing mode." t)
(defun md-extra-stuff ()
  (auto-fill-mode 1))
(add-hook 'markdown-mode-hook 'md-extra-stuff)

;; PYTHON !
(add-to-list 'interpreter-mode-alist '("python2.5" . python-mode))
(defun mypy-extra-stuff ()
  (setq show-trailing-whitespace t)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (define-key python-mode-map "\C-m" 'newline-and-indent))

  ;(local-set-key [return] 'newline-and-indent))
;;(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook 'mypy-extra-stuff)

;;;; `Cython' mode.
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(define-derived-mode cython-mode python-mode "Cython"
  (font-lock-add-keywords
   nil
   `((,(concat "\\<\\(NULL"
               "\\|c\\(def\\|har\\|typedef\\)"
               "\\|e\\(num\\|xtern\\)"
               "\\|float"
               "\\|in\\(clude\\|t\\)"
               "\\|object\\|public\\|struct\\|type\\|union\\|void"
               "\\)\\>")
      1 font-lock-keyword-face t))))

;; GOOGLE GO MODE
(require 'go-mode-load)

(defun krig-go-mode-hook ()
  (setq show-trailing-whitespace t)
  (turn-on-fic-mode)
  (subword-mode 1)
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
  (local-set-key [return] 'newline-and-indent))


(dolist (hook '(
                go-mode-hook
                ))
  (add-hook hook 'krig-go-mode-hook))

;; IMPROVED JAVASCRIPT MODE
;; (autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; (load "js2-setup.el")
;; (setq js2-use-font-lock-faces t)

;; (defun krig-js2-style-fix ()
;;   (setq show-trailing-whitespace t))
;; (add-hook 'js2-mode-hook 'krig-js2-style-fix)
;; (add-hook 'js-mode-hook 'krig-js2-style-fix)

;; MUSTACHE MODE
;;(add-to-list 'load-path "~/.emacs.d/mustache-mode.el")
(require 'mustache-mode)


;; RUBY HTML.ERB MODE
;; (setq
;;  nxhtml-global-minor-mode t
;;  mumamo-chunk-coloring 5
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  nxml-degraded t)
;;(setq mumamo-chunk-coloring 3)
;;(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; HAML-MODE
(require 'haml-mode)

;; ORG-MODE
(setq org-startup-indented t)

;; GIT-MESSENGER
(when (require 'git-messenger nil 'noerror)
  (setq git-messenger:show-detail t)
  (define-key git-messenger-map (kbd "C-k") 'git-messenger:copy-message)
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message))

;; FLYMAKE

(require 'flymake)
(setq flymake-gui-warnings-enabled nil)
(setq help-at-pt-timer-delay 0.9)
(setq help-at-pt-display-when-idle '(flymake-overlay))
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
;;(setq flymake-log-level 0)


(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list (expand-file-name "~/.emacs.d/flymake-haskell.pl")
        (list source base-dir)))

(progn
  (push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
        flymake-allowed-file-name-masks)
  (push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
        flymake-allowed-file-name-masks)
  (push
   '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
     1 2 3 4) flymake-err-line-patterns))

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 3))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

;;(add-hook
;; 'haskell-mode-hook
;; '(lambda ()
;;    (if (not (null buffer-file-name)) (flymake-mode))))

;; don't enable flymake if running over tramp
(defun aw-flymake-if-buffer-isnt-tramp ()
  (if (not (and (boundp 'tramp-file-name-structure)
                (string-match (car tramp-file-name-structure) (buffer-file-name))))
      (flymake-mode t)))

;; enables flymake mode iff buffer has a filename set,
;; otherwise things breaks badly for things such as emerge
(defun aw-flymake-if-buffer-has-filename ()
  (if (buffer-file-name)
      (aw-flymake-if-buffer-isnt-tramp)))

(defun aw-flymake-get-err ()
  "Gets first error message for current line"
  (let ((fm-err (car (flymake-find-err-info flymake-err-info (flymake-current-line-no)))))
    (if fm-err
        (flymake-ler-text (nth 0 fm-err)))))

(defun aw-flymake-display-err ()
  (interactive)
  (let ((err (aw-flymake-get-err)))
    (message (format "FLYMAKE: %s" err))))

(defmacro aw-flymake-add-simple (ptrn cmd)
  `(add-to-list 'flymake-allowed-file-name-masks
                (list ,ptrn
                      (lambda ()
                        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                           'flymake-create-temp-inplace))
                               (local-file (file-relative-name
                                            temp-file
                                            (file-name-directory buffer-file-name))))
                          (list ,cmd (list local-file)))))))

;; enable flymake on c
;;(add-hook 'c-mode-hook 'aw-flymake-if-buffer-has-filename t)
;; enable flymake on py
;;(add-hook 'python-mode-hook 'aw-flymake-if-buffer-has-filename t)

;; Or lets do a global enable global enable
;;(add-hook 'find-file-hook 'aw-flymake-if-buffer-has-filename)

;; For problems with PATH on mac os X, see
;; http://stackoverflow.com/questions/9388315/how-to-make-emacs-pickup-the-correct-python-in-snow-leopard/9388555#9388555
;;(setenv "PYMACS_PYTHON" "/usr/local/bin/python2.7")
;;(package-initialize)
;;(require 'pymacs)
;;(setq python-check-command (expand-file-name "~/bin/python-check.sh"))
;;(elpy-enable)
;;(elpy-use-ipython)
;;
;;(pymacs-load "ropemacs" "rope-")

;; Pyflakes for python
;;(require 'flymake)
;;(defun flymake-pychecker-init ()
;;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;; 	 (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;     (list (expand-file-name "~/bin/pepflakes") (list local-file))))
;; (add-to-list 'flymake-allowed-file-name-masks
;; 	     '("\\.py\\'" flymake-pychecker-init))

(when (require 'flymake-python-pyflakes nil 'noerror)
  (setq flymake-python-pyflakes-executable "flake8")
  (setq flymake-python-pyflakes-extra-arguments '("--max-complexity=10" "--max-line-length=99"))
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))
;; Forces flymake to underline bad lines, instead of fully
;; highlighting them; remove this if you prefer full highlighting.
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "#aa3333"))))
 '(flymake-warnline ((((class color)) (:underline "#aa4444")))))

(require 'ipython nil 'noerror)

;; ELPY
(when (boundp 'elpy-enable)
  (elpy-enable))

;; SLIME

(load (expand-file-name "~/quicklisp/slime-helper.el"))

(add-to-list 'load-path "~/.emacs.d/slime/")  ; your SLIME directory
(setq inferior-lisp-program "sbcl --noinform") ; your Lisp system
(require 'slime)
(slime-setup '(slime-fancy))

;; QUACK

(require 'quack)

(autoload 'bison-mode "bison-mode.el")
(add-to-list 'auto-mode-alist '("\\.ypp\\'" . bison-mode))

(autoload 'flex-mode "flex-mode.el")
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))

;;(load "~/.emacs.d/haskell-mode-2.8.0/haskell-site-file")
(load "~/.emacs.d/haskell-mode/haskell-site-file")
;;sadly doesn't work as it should
;;(setq haskell-ghci-comint-prompt-regexp "^[[:nonascii:]]> ")

(defun krig-haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (imenu-add-menubar-index)
  (subword-mode 1))

(add-hook 'haskell-mode-hook 'krig-haskell-mode-hook)

(add-to-list 'completion-ignored-extensions ".hi")

;; YASNIPPET
(require 'yasnippet)
(yas-global-mode 1)

;; ;; PARROT

;; (progn
;;   (load "parrot")
;;   (load "pasm")
;;   (add-to-list 'auto-mode-alist '("\\.pasm\\'" . pasm-mode))
;;   (add-hook 'pasm-mode-hook
;;             (function (lambda ()
;;                         (setq indent-tabs-mode nil))))
;;   (autoload 'pir-mode "pir-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.pir\\'" . pir-mode)))


;; MAGIT
(when (require 'magit nil 'noerror)
  (global-set-key (kbd "C-x C-a") 'magit-status))

;; IDO RECENTF
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))
(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

(setq imenu-auto-rescan t)
(autoload 'idomenu "idomenu" nil t)
(global-set-key "\C-x\C-\\" 'idomenu)

(defun zap-space-forward () ; adapted from `delete-horizontal-space'
  "*Delete all spaces, tabs and newlines after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

(global-set-key [C-tab] (lambda () (interactive) (insert-char 9 1)))

;;(require 'undo-tree)
;;(global-undo-tree-mode)

;; SPEEDBAR
(progn
  (load "sr-speedbar.el")
  (setq speedbar-use-images nil)
  (make-face 'speedbar-face)
  (if (string-match "kowloon" hostname)
      (set-face-font 'speedbar-face "Ubuntu Mono-12")
    (set-face-font 'speedbar-face "Ubuntu Mono-9"))
  (setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))
  (sr-speedbar-refresh-turn-on)
  (speedbar-add-supported-extension ".hs")
  (global-set-key (kbd "M-p") 'sr-speedbar-toggle))

;; UNBOUND
(progn
  (require 'unbound))

;; NOTMUCH

(defun krig-notmuch-mark-as-read ()
  "notmuch: mark as read toggle"
  (interactive)
  (notmuch-search-tag
   (if (member "unread" (notmuch-search-get-tags))
       "-unread" "+unread")))

(defun my-notmuch-show-view-as-patch ()
  "View the the current message as a patch."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (notmuch-get-bodypart-internal id 1 nil)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
                 (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

(when (require 'notmuch nil 'noerror)
  (setq mail-user-agent 'message-user-agent)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq message-sendmail-envelope-from 'header)
  (setq message-kill-buffer-on-exit t)
  ;(setq notmuch-search-oldest-first nil) ;; second thought on this :P
  (define-key 'notmuch-show-mode-map "D" 'my-notmuch-show-view-as-patch)
  (require 'notmuch-address)
  (setq notmuch-address-command (expand-file-name "~/bin/nottoomuch-addresses.sh"))
  (notmuch-address-message-insinuate))

;; SMEX
(progn
  (add-to-list 'load-path "~/.emacs.d/smex")
  (require 'smex)
  (setq smex-save-file "~/.emacs.d/smex.save")
  (setq smex-prompt-string "M-x ")
  (smex-initialize)
  (smex-auto-update) ;; refresh caches after 60 seconds of idle time
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s." new-name)))

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond
   ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun set-tab-stop-width (width)
  "Set all tab stops to WIDTH in current buffer.
    This updates `tab-stop-list', but not `tab-width'.
    By default, `indent-for-tab-command' uses tabs to indent, see
    `indent-tabs-mode'."
  (interactive "nTab width: ")
  (let* ((max-col (car (last tab-stop-list)))
         ;; If width is not a factor of max-col,
         ;; then max-col could be reduced with each call.
         (n-tab-stops (/ max-col width)))
    (set (make-local-variable 'tab-stop-list)
         (mapcar (lambda (x) (* width x))
                 (number-sequence 1 n-tab-stops)))
    ;; So preserve max-col, by adding to end.
    (unless (zerop (% max-col width))
      (setcdr (last tab-stop-list)
              (list max-col)))))

(defun un-camelcase-string (s &optional sep start)
    "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is an underscore \"_\".
    If third argument START is non-nil, convert words after that
    index in STRING."
    (let ((case-fold-search nil))
      (while (string-match "[A-Z]" s (or start 1))
        (setq s (replace-match (concat (or sep "_")
                                       (downcase (match-string 0 s)))
                               t nil s)))
      (downcase s)))

(defun snakecase-region (start end)
  "Changes region from camelCase to snake_case"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (let ((s (un-camelcase-string (buffer-substring (point-min) (point-max)))))
                      (delete-region (point-min) (point-max))
                      (goto-char (point-min))
                      (insert s))))

(defun snakecase-word-or-region ()
  "Changes word or region from camelCase to snake_case"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (snakecase-region pos1 pos2)))

(global-set-key (kbd "C-c C--") 'snakecase-word-or-region)

(put 'ido-exit-minibuffer 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun mrc-dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;; IGNORAMUS
(require 'ignoramus)
(ignoramus-setup)

(require 'whitespace)

(defun krig-twitter-time ()
  "time to compose a tweet"
  (interactive)
  (make-local-variable 'whitespace-style)
  (make-local-variable 'whitespace-line-column)
  (setq whitespace-line-column 140)
  (setq whitespace-style '(face tabs lines-tail trailing))
  (whitespace-mode 1)
  (auto-fill-mode 0))

(defun now ()
  "Insert string for the current time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

;; THEME

;; load-theme advice by nano
(defadvice load-theme (after fixup-colors activate)
  (pcase (symbol-name (ad-get-arg 0))
    ("zenburn" (progn
               (set-face-background 'whitespace-tab "#353535")
               (set-face-background 'whitespace-trailing "#ff0000")
               (set-face-background 'hl-line "#353535")
               (set-face-underline 'hl-line  nil)
               (set-face-background 'idle-highlight (face-background 'default))
               (set-face-foreground 'idle-highlight "#ff8900")
               (set-face-foreground 'show-paren-match-face "#ff8900")))
    ("solarized-light" (progn
                       (set-face-background 'whitespace-tab "#353535")
                       (set-face-background 'whitespace-trailing "#ff0000")
                       (set-face-background 'hl-line "#e5dfcf")
                       (set-face-underline 'hl-line  nil)
                       (set-face-background 'idle-highlight "#ffffff")
                       (set-face-foreground 'idle-highlight "#ff8900")
                       (set-face-foreground 'show-paren-match-face "#ff8900")))
    ("noctilux" (progn
                  (set-face-background 'mode-line "#101010")
                  (set-face-foreground 'mode-line "#999999")
                  (set-face-inverse-video 'mode-line nil)
                  (set-face-background 'mode-line-inactive "#121414")
                  (set-face-foreground 'mode-line-inactive "#333333")
                  (set-face-inverse-video 'mode-line-inactive nil)))))


;;(add-hook 'after-init-hook '(lambda () (load-theme 'github)))
;;(add-hook 'after-init-hook '(lambda () (load-theme 'wombat)))
(add-hook 'after-init-hook '(lambda ()
                              (load-theme 'noctilux)
                              (scroll-bar-mode -1)))
                              ;;(load-theme 'flatui)))

(defun switch-to-flatui ()
  "disable noctilux, enable flatui"
  (interactive)
  (disable-theme 'noctilux)
  (load-theme 'flatui))

(defun switch-to-noctilux ()
  "enable noctilux, disable flatui"
  (interactive)
  (disable-theme 'flatui)
  (load-theme 'noctilux))

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(defun open-worklog ()
  "creates the worklog for today if it doesn't exist and opens it in emacs"
  (interactive)
  (let ((fn (shell-command-to-string "sh ~/annex/doc/work/SUSE/create-entry.sh")))
    (find-file (s-trim fn))))

(find-file "~/.todo")
(rename-buffer "*todo*")
