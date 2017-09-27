;;----------------------------------------------------------------------------
;; Global Package Initializations
;;----------------------------------------------------------------------------

;; Use-package loads packages only when packages are loaded
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Loads $PATH when Emacs is opened via gui
(push "/usr/local/bin" exec-path)

(require 'use-package)

(load "defuns")
(load "neptune-theme")

;;----------------------------------------------------------------------------
;; Global hooks :-)
;;----------------------------------------------------------------------------

;; (add-hook 'after-change-major-mode-hook 'fci-mode)

;;----------------------------------------------------------------------------
;; Global settings for all modes
;;----------------------------------------------------------------------------

;; Indicate where the 100 column character ends
(require 'fill-column-indicator)
(setq fci-rule-use-dashes nil)
(setq fci-rule-color "#474e90")
(setq-default fci-rule-column 100)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; Make the region act quite like the text "highlight" in many apps.
(setq transient-mark-mode t)

;; Default tabs/indents are 2 spaces
(setq-default tab-width 2)
(setq tab-width 2)
(setq standard-indent 2)

;; No backups
(setq make-backup-files  nil)

;; No .saves files
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix
      "~/.emacs.d/.cache/auto-save-list/.saves-")

;; Scroll to where you last left off
;; (require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")
(save-place-mode 1)
;; (setq-default save-place t)

;; Make yes/no options y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup settings
(setq auto-fill-mode 1)
(setq auto-save-mode nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Misc settings
(setq indicate-buffer-boundaries nil)


;; This will help distinguish files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Bookmarks are good.  I need to use them more.
(setq
 bookmark-default-file (dot-emacs ".bookmarks")
 bookmark-save-flag 1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

                                        ; Auto associate files that emacs doesn't know what to do with otherwise
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("hosts" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Cask" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".*mutt.*" . message-mode) auto-mode-alist))

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; Set column with
(setq fill-column 80)
(setq-default fill-column 80)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; Just say no to splash screens
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Turn off tool bar, scroll bar, and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

;; Highlight the current line
(use-package highline
  :init
  (progn
    (global-hl-line-mode)
    ))

;;----------------------------------------------------------------------------
;; Change the default colors for matching parens
;;----------------------------------------------------------------------------

(use-package paren
  :init
  (progn
    (show-paren-mode)
    (set-face-background 'show-paren-match "#1d1f21")
    (set-face-foreground 'show-paren-match "#dd0093")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
    ))

;;------------------------------------------------------------------------------
;; Smex - Enhanced M-x
;;------------------------------------------------------------------------------
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :init
  (progn
    (setq smex-save-file (dot-emacs ".smex.save"))
    (smex-initialize)))

;; A little smarter when hitting RET
(use-package smart-newline
  :bind (("RET" . smart-newline))
  :init
  (progn (smart-newline-mode 1) ))

;;------------------------------------------------------------------------------
;; IDO options
;;------------------------------------------------------------------------------

;; Do not auto complete when creating new directories!
(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))

;; Interactively Do Things
(use-package ido
  :init
  (progn
    ;; Maps Ctrl-j amd Ctrl-k to up an down
    (add-hook 'ido-setup-hook
              (lambda ()
                (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
                (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
                ))

    (ido-mode 1)
    (ido-disable-line-trucation))
  (setq ido-decorations (quote ("\n↪ "     "" "\n   " "\n   ..." "[" "]"
                                " [No match]" " [Matched]" " [Not readable]"
                                " [Too big]" " [Confirm]")))
  (setq ido-ignore-buffers
        '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*Ibuffer"
          "^\*scratch*" "^\*TAGS" "^session\.*" "^\*", "^\*helm-projectile*","^\*magit*"))
  :config
  (progn
    (setq ido-everywhere t)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; Flx Fuzzy file finder integration with IDO
(use-package flx-ido
  :init (flx-ido-mode 1))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(use-package ido-completing-read)

;; Yet another paste tool, this one for Gist (awesome)
(use-package gist)

;; Keep a history of recent changes
(use-package recentf
  :init
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 60)))

;;----------------------------------------------------------------------------
;; Silver Searcher searching (Say that 3 times in a row)
;;----------------------------------------------------------------------------

(use-package ag
  :init
  (progn
    (setq ag-highlight-search t)))


;;----------------------------------------------------------------------------
;; Projectile for project file navigation
;;----------------------------------------------------------------------------

(use-package projectile
  :defer 1
  :config
  (progn
    (projectile-global-mode)
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    (setq projectile-cache-file (expand-file-name ".projectile.cache" user-emacs-directory))
    (setq projectile-known-projects-file (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))
    (setq projectile-sort-order (quote recently-active))
    (setq projectile-enable-caching t)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    )
  )

;; Adds projectile helpers functions for Rails projects
(use-package projectile-rails
  :defer 1)

;;----------------------------------------------------------------------------
;; Helm for enhanced project file navigation
;;----------------------------------------------------------------------------

(use-package helm
  :defer 1
  :init
  (progn

    ;; hide minibuffer in Helm session, since we use the header line already
    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t
          helm-ff-newfile-prompt-p nil
          helm-ff-skip-boring-files t
          helm-mp-highlight-delay 0.3
          helm-display-function 'pop-to-buffer
          helm-echo-input-in-header-line t
          helm-candidate-number-limit 50
          helm-autoresize-max-height 20
          helm-autoresize-min-height 20
          )

    ;;fuzzy matching setting
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-file-cache-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-locate-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-buffers-fuzzy-matching t)
    )
  :config
  (progn

    (helm-mode +1)
    (helm-autoresize-mode 1)
    (set-face-attribute 'helm-selection nil :background "purple" :foreground "black")
    (setq helm-display-header-line nil)

    (defvar dotspacemacs-editing-style 'vim
      "Either `vim' or `emacs'. Evil is always enabled but if the variable
is `emacs' then the `holy-mode' is enabled at startup.")

    ;; helm navigation on hjkl
    (defun spacemacs//helm-hjkl-navigation (&optional arg)
      "Set navigation in helm on `jklh'. ARG non nil means that the editing style is `vim'."
      (cond
       (arg
        (define-key helm-map (kbd "C-j") 'helm-next-line)
        (define-key helm-map (kbd "C-k") 'helm-previous-line)
        (define-key helm-map (kbd "C-h") 'helm-next-source)
        (define-key helm-map (kbd "C-l") 'helm-previous-source))
       (t
        (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
        (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
        (define-key helm-map (kbd "C-h") nil)
        (define-key helm-map (kbd "C-l") 'helm-recenter-top-bottom-other-window))))
    (spacemacs//helm-hjkl-navigation (eq 'vim dotspacemacs-editing-style))
    )

  (defvar bottom-buffers nil
    "List of bottom buffers before helm session.
    Its element is a pair of `buffer-name' and `mode-line-format'.")

  (defun bottom-buffers-init ()
    (setq-local mode-line-format (default-value 'mode-line-format))
    (setq bottom-buffers
          (cl-loop for w in (window-list)
                   when (window-at-side-p w 'bottom)
                   collect (with-current-buffer (window-buffer w)
                             (cons (buffer-name) mode-line-format)))))


  (defun bottom-buffers-hide-mode-line ()
    (setq-default cursor-in-non-selected-windows nil)
    (mapc (lambda (elt)
            (with-current-buffer (car elt)
              (setq-local mode-line-format nil)))
          bottom-buffers))


  (defun bottom-buffers-show-mode-line ()
    (setq-default cursor-in-non-selected-windows t)
    (when bottom-buffers
      (mapc (lambda (elt)
              (with-current-buffer (car elt)
                (setq-local mode-line-format (cdr elt))))
            bottom-buffers)
      (setq bottom-buffers nil)))

  (defun helm-keyboard-quit-advice (orig-func &rest args)
    (bottom-buffers-show-mode-line)
    (apply orig-func args))


  (add-hook 'helm-before-initialize-hook #'bottom-buffers-init)
  (add-hook 'helm-after-initialize-hook #'bottom-buffers-hide-mode-line)
  (add-hook 'helm-exit-minibuffer-hook #'bottom-buffers-show-mode-line)
  (add-hook 'helm-cleanup-hook #'bottom-buffers-show-mode-line)
  (advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)
  (setq helm-display-header-line nil)

  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

  (defun helm-toggle-header-line ()
    (if (> (length helm-sources) 1)
        (set-face-attribute 'helm-source-header
                            nil
                            :foreground helm-source-header-default-foreground
                            :background helm-source-header-default-background
                            :box helm-source-header-default-box
                            :height 1.0)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground (face-attribute 'helm-selection :background)
                          :background (face-attribute 'helm-selection :background)
                          :box nil
                          :height 0.1)))


  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  )

(use-package helm-projectile
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile
             helm-projectile-switch-project)
  :init
  (progn
    (setq projectile-switch-project-action 'helm-projectile)
    (setq projectile-completion-system 'helm)
    ;;(helm-projectile-on)
    )
  )

(use-package helm-fuzzier
  :defer t
  :init
  (progn))

(use-package helm-flx
  :defer t
  :init
  (progn))


(use-package helm-ag
  :defer t
  :init
  (progn
    ))

;; Yaml
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; CSV files
(use-package ses-mode
  :mode ("\\.csv$" . ses-mode))

;; Spell checker
(use-package flyspell
  :init
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-flyspell)))

;;----------------------------------------------------------------------------
;; Smooth scrolling
;;----------------------------------------------------------------------------

(use-package smooth-scrolling
  :init
  (setq redisplay-dont-pause t
        scroll-margin 1
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1))

;;----------------------------------------------------------------------------
;; Indicate syntax errors
;;----------------------------------------------------------------------------

;; (use-package flycheck
;;   :init
;;   (progn
;;     (setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
;;     (add-hook 'ruby-mode-hook 'flycheck-mode)
;;     (add-hook 'emacs-lisp 'flycheck-mode)
;;     ))

;;----------------------------------------------------------------------------
;; Evil mode - uses modal editing key commands
;;----------------------------------------------------------------------------
(use-package evil
  :init
  (progn
    (evil-mode 1)

    ;; (define-key evil-normal-state-map (kbd "/") 'swiper)
    (define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)
    (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-visual-state-map (kbd "C-]") 'text-shift-right)
    (define-key evil-visual-state-map (kbd "C-[") 'text-shift-left)
    (define-key evil-insert-state-map [remap newline] 'evil-ret-and-indent)

    ;; Make a escape actually quit
    ;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (global-set-key [escape] 'keyboard-quit)

    (setq evil-want-fine-undo t)
    (setq evil-default-cursor t)
    (setq evil-normal-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("orange" bar))

    (defun set-mode-to-default-emacs (mode)
      (evil-set-initial-state mode 'emacs))

    (add-hook 'compilation-mode-hook '(lambda ()
                                        (local-unset-key "g")
                                        (local-unset-key "h")
                                        (local-unset-key "k")))

    (add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))

    (mapcar 'set-mode-to-default-emacs
            '(dired
              magit-branch-manager-mode
              comint-mode
              magit-log-mode
              eshell-mode
              diff-mode))

    (lexical-let ((default-color (cons (face-background 'mode-line)
                                       (face-foreground 'mode-line))))
      (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color)))
      )))


;; Make a visual selection with v or V, and then hit * to search forward
(use-package evil-visualstar)

(use-package evil-exchange
  :init
  (evil-exchange-install))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :init
  (evil-commentary-mode))

(use-package evil-numbers)
(use-package evil-rails)
(use-package evil-magit)

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode 1))

(use-package evil-leader
  :init
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "t"   'projectile-find-file
      "o"   'dired
      "b"   'projectile-switch-to-buffer
      "i"   'iwb
      ","   'switch-to-previous-buffer
      "."   'dumb-jump-go
      "TAB" 'projectile-ibuffer
      "/"   'helm-projectile-ag
      "w"   'matts-close-and-delete-window
      "x"   'smex
      "X"   'smex-major-mode-commands
      "k"   'matts-close-and-delete-window
      "s"   'matts-find-symbol
      "c"   'evil-commentary-line
      "R"   'matts-ido-choose-from-recentf
      "a"   'annotate-annotate

      "gb" 'magit-blame
      "gl" 'magit-log-all
      "gL" 'magit-log-buffer-file
      "gs" 'magit-status
      "gC" 'magit-commit

      "rm"  'projectile-rails-find-model
      "rc"  'projectile-rails-find-controller
      "rv"  'projectile-rails-find-view
      "rt"  'projectile-find-test-file
      "rh"  'projectile-rails-goto-schema
      "rr"  'projectile-rails-goto-routes
      "rm"  'projectile-rails-goto-gemfile
      "rg"  'projectile-rails-find-migration
      "rs"  'projectile-rails-find-stylesheet
      "rj"  'projectile-rails-find-javascript
      "rl"  'projectile-rails-find-lib

      "r.m" 'projectile-rails-find-current-model
      "r.t" 'projectile-rails-find-current-test
      "r.g" 'projectile-rails-find-current-migration

      ":" 'ruby-tools-to-symbol
      "'" 'ruby-tools-to-single-quote-string

      "rdb" 'my-rails-database

      "et"  'alchemist-mix-test
      "ec"  'alchemist-mix-compile
      "ex"  'alchemist-iex-project-run
      "er"  'alchemist-iex-project-run
      "ee"  'alchemist-eval-print-current-line

      "m"   'list-bookmarks
      "p"   'matts-ido-find-project
      "]"   'text-shift-right
      "["   'text-shift-left
      "#"   'flyspell-auto-correct-word
      )))

;;----------------------------------------------------------------------------
;; Hulk smash keyboard (to run commands)
;;----------------------------------------------------------------------------

(use-package key-chord
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-define-global "jk" 'my-save-if-bufferfilename)
    ))

;;----------------------------------------------------------------------------
;; Make copy and paste actually work
;;----------------------------------------------------------------------------

(use-package pbcopy
  :init
  (progn
    (turn-on-pbcopy)
    ))

;;----------------------------------------------------------------------------
;; Dired (Directory Edit) mode
;;----------------------------------------------------------------------------

(use-package dired
  :init
  (progn
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
    (put 'dired-find-alternate-file 'disabled nil)
    )
  :config
  (progn
    (use-package wdired)
    (use-package dired-x)
    (use-package dired-details)
    (dired-details-install)
    (setq dired-omit-files
          (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                  (seq "~" eol)                 ;; backup-files
                  (seq "coverage" eol)          ;; code coverage files
                  )))
    (setq-default dired-details-hidden-string "--- ")
    (setq dired-omit-extensions
          (append dired-latex-unclean-extensions
                  dired-bibtex-unclean-extensions
                  dired-texinfo-unclean-extensions))
    (setq dired-details-hidden-string "[ ... ] ")
    (setq dired-listing-switches "-l")
    (setq directory-free-space-args "-kh")
    (setq dired-omit-size-limit nil)
    (setq dired-dwim-target t)
    (setq dired-recursive-deletes 'top)
    (setq dired-recursive-copies (quote always))

    (define-key dired-mode-map [delete] 'dired-do-delete)
    (define-key dired-mode-map (kbd "DEL") 'matts-dired-up-directory)
    (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
    (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
    (define-key dired-mode-map "n" 'dired-touch-now)
    (define-key dired-mode-map "o" 'matts-dired-open-mac)
    (define-key dired-mode-map [return] 'dired-find-alternate-file)
    ;; (define-key dired-mode-map [return] 'toggle-diredp-find-file-reuse-dir)
    ))

;;----------------------------------------------------------------------------
;; Markdown mode options
;;----------------------------------------------------------------------------

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         (".simplenote$" . markdown-mode)))

(use-package markdown-mode+)

(use-package adoc-mode
  :mode (("\\.adoc$" . adoc-mode)))

;;----------------------------------------------------------------------------
;; Clojure mode
;;----------------------------------------------------------------------------

(use-package clojure-mode)

;;----------------------------------------------------------------------------
;; Elixir and related modes
;;----------------------------------------------------------------------------

(use-package elixir-mode
  :init
  (progn
    (use-package alchemist)
    ))

;;----------------------------------------------------------------------------
;; Web mode
;;----------------------------------------------------------------------------

(use-package web-mode
  :mode (("\\.ejs\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.eex\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-extra-snippets '(("erb" . (("link_to" . "<%- | %>") ("b=" . "<%= | %>"))) ))
    )
  :init
  (progn
    (add-hook 'web-mode-hook
              (lambda ()
                (whitespace-mode -1)
                (fci-mode -1)
                (fic-ext-mode -1)
                ))
    )
  )

;;----------------------------------------------------------------------------
;; Prettyifies javascript, html, and css files.
;;----------------------------------------------------------------------------

(use-package web-beautify
  :init
  (progn
    (eval-after-load 'js-mode
      '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'web-mode
      '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
    ))

;;----------------------------------------------------------------------------
;; Coffeescript mode
;;----------------------------------------------------------------------------

(use-package coffee-mode
  :config
  (progn
    (set (make-local-variable 'tab-width) 2)
    (setq coffee-js-mode 'javascript-mode)
    (setq coffee-args-compile '("-c" "--no-wrap"))
    (setq coffee-debug-mode t)
    (setq coffee-command "/usr/local/bin/coffee")
    )
  :mode (("\\.coffee$" . coffee-mode)
         ("Cakefile$" . coffee-mode)))

;;----------------------------------------------------------------------------
;; Javascript with js-mode
;;----------------------------------------------------------------------------

(use-package js-mode
  :mode ("\\.js$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2))))
  :config
  (setq js-indent-level 2))


;;----------------------------------------------------------------------------
;; JSON with json-mode
;;----------------------------------------------------------------------------

(use-package json-mode
  :mode ("\\.json$" . json-mode))


;;----------------------------------------------------------------------------
;; Vue.js mode
;;----------------------------------------------------------------------------

(use-package vue-mode)
(use-package vue-html-mode)

;;----------------------------------------------------------------------------
;; Magit - awesome Git integration
;;----------------------------------------------------------------------------

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (setq yas/dont-activate t)

    ;; make magit status go full-screen but remember previous window
    ;; settings from: http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    ;; Close popup when commiting - this stops the commit window
    ;; hanging around
    ;; From: http://git.io/rPBE0Q
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))

    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))

    ;; these two force a new line to be inserted into a commit window,
    ;; which stops the invalid style showing up.
    ;; From: http://git.io/rPBE0Q
    (defun magit-commit-mode-init ()
      (when (looking-at "\n")
        (open-line 1)))

    (add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

    ;; (set-face-background 'magit-item-highlight "#0C1021")
    ;; (set-face-foreground 'magit-diff-add "#79b958")
    ;; (set-face-foreground 'magit-diff-del "#d95c47")
    )
  :config
  (progn

    ;; restore previously hidden windows
    (defadvice magit-quit-window (around magit-restore-screen activate)
      (let ((current-mode major-mode))
        ad-do-it
        ;; we only want to jump to register when the last seen buffer
        ;; was a magit-status buffer.
        (when (eq 'magit-status-mode current-mode)
          (jump-to-register :magit-fullscreen))))

    (defun magit-maybe-commit (&optional show-options)
      "Runs magit-commit unless prefix is passed"
      (interactive "P")
      (if show-options
          (magit-key-mode-popup-committing)
        (magit-commit)))

    (define-key magit-mode-map "c" 'magit-maybe-commit)


    (setq
     ;; use ido to look for branches
     magit-completing-read-function 'magit-ido-completing-read
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t
     ;; ask me if I want to include a revision when rewriting
     magit-rewrite-inclusive 'ask
     ;; ask me to save buffers
     magit-save-some-buffers t
     ;; pop the process buffer if we're taking a while to complete
     magit-process-popup-time 10
     ;; ask me if I want a tracking upstream
     magit-set-upstream-on-push 'askifnotset
     )

    (setq git-commit-summary-max-length 999)

    ;; (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    ;; (setq magit-set-upstream-on-push t)
    ;; (setq magit-completing-read-function 'magit-ido-completing-read)
    ;; (setq magit-stage-all-confirm nil)
    ;; (setq magit-unstage-all-confirm nil)
    )
  :bind ("C-x g" . magit-status))


(use-package browse-at-remote)

;;----------------------------------------------------------------------------
;; Git Gutter indicates line changes (+ -) in a file in the fringe
;;----------------------------------------------------------------------------

(use-package git-gutter
  :init
  (global-git-gutter-mode t)
  :config
  (progn
    (setq git-gutter:lighter " gg")
    (setq git-gutter:window-width 1)
    (setq git-gutter:modified-sign ".")
    (setq git-gutter:added-sign "+")
    (setq git-gutter:deleted-sign "-")
    (set-face-foreground 'git-gutter:added "#daefa3")
    (set-face-foreground 'git-gutter:deleted "#FA8072")
    (set-face-foreground 'git-gutter:modified "#b18cce")
    ))

;;----------------------------------------------------------------------------
;; ERC mode for IRC in emacs
;;----------------------------------------------------------------------------

(use-package erc
  :config
  (progn
    (setq erc-autojoin-channels-alist '(("freenode.net"))
          erc-nick "mattsears"
          erc-server "irc.freenode.net"
          erc-away-nickname "mattsears_AWAY"
          erc-user-full-name "Matt Sears")
    (setq erc-prompt (lambda ()
                       (if (and (boundp 'erc-default-recipients) (erc-default-target))
                           (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
                         (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t))))))

;;----------------------------------------------------------------------------
;; Ruby and related  modes
;;----------------------------------------------------------------------------


(use-package ruby-mode
  :init
  (progn
    ;; (use-package ruby-tools
    ;;   :ensure t)

    (use-package inf-ruby
      :init
      (progn
        (inf-ruby-minor-mode)))

    ;; Auto detects the ruby verion for projects using Rbenv
    (use-package rbenv
      :init
      (progn (global-rbenv-mode)))

    ;; Provides helpers for converting hashes to the newer syntax (eg {key: value})
    (use-package ruby-hash-syntax))

  :config
  (progn
    ;; (add-hook 'ruby-mode-hook 'matt/toggle-wrap)
    ;; (add-hook 'ruby-mode-hook #'rubocop-mode)
    ;; (add-hook 'ruby-mode-hook 'global-company-mode)
    (add-hook 'ruby-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
    (setq ruby-deep-arglist nil)
    (setq ruby-dbg-flags "-W0")
    (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
    (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

    ;; Goddammit ruby indention
    (setq ruby-align-to-stmt-keywords '(begin if while unless until case for def))
    (setq ruby-deep-indent-paren nil)
    (setq ruby-deep-indent-paren-style nil)
    (setq ruby-align-chained-calls nil)
    (setq ruby-deep-indent-paren nil)
    (setq ruby-deep-indent-paren-style nil)
    (setq ruby-use-smie nil)

    (defadvice ruby-indent-line (after unindent-closing-paren activate)
      "Indent sole parenthesis in loca's way."
      (let ((column (current-column))
            indent offset)
        (save-excursion
          (back-to-indentation)
          (let ((state (syntax-ppss)))
            (setq offset (- column (current-column)))
            (when (and (eq (char-after) ?\))
                       (not (zerop (car state))))
              (goto-char (cadr state))
              (setq indent (current-indentation)))))
        (when indent
          (indent-line-to indent)
          (when (> offset 0) (forward-char offset)))))

    ;; (defun add-rails-keywords-hook ()
    ;;   (font-lock-add-keywords nil
    ;;                           '(("\\has_many\\" (0 font-lock-warning-face)))))
    ;; (add-hook 'ruby-mode-hook #'add-rails-keywords-hook)

    ;; (defvar rails-mode-keywords
    ;;   '("default_scope" "named_scope" "scope" "serialize" "belongs_to" "has_one"
    ;;     "has_many" "has_and_belongs_to_many" "composed_of" "accepts_nested_attributes_for"
    ;;     "before_create" "before_destroy" "before_save" "before_update" "before_validation"
    ;;     "before_validation_on_create" "before_validation_on_update" "after_create"
    ;;     "after_destroy" "after_save" "after_update" "after_validation"
    ;;     "after_validation_on_create" "after_validation_on_update" "around_create"
    ;;     "around_destroy" "around_save" "around_update" "after_commit" "after_find"
    ;;     "after_initialize" "after_rollback" "after_touch" "attr_accessible"
    ;;     "attr_protected" "attr_readonly" "validates" "validate" "validate_on_create"
    ;;     "validate_on_update" "validates_acceptance_of" "validates_associated"
    ;;     "validates_confirmation_of" "validates_each" "validates_exclusion_of"
    ;;     "validates_format_of" "validates_inclusion_of" "validates_length_of"
    ;;     "validates_numericality_of" "validates_presence_of" "validates_size_of"
    ;;     "validates_existence_of" "validates_uniqueness_of" "validates_with"
    ;;     "enum" "after_create_commit" "after_update_commit" "after_destroy_commit")
    )
  :bind (("C-{" . ruby-toggle-hash-syntax))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Berkshelf$" . ruby-mode)
         ("Procfile$" . ruby-mode)
         ("Procfile-dev$" . ruby-mode)
         ("Gemfile.lock$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

;;----------------------------------------------------------------------------
;; Css/Sass/Haml modes
;;----------------------------------------------------------------------------

(use-package css-mode
  :init
  (progn
    (hexcolour-add-to-font-lock))
  :config
  (progn
    (setq cssm-indent-level 2)
    (setq cssm-newline-before-closing-bracket t)
    (setq cssm-indent-function #'cssm-c-style-indenter)

    (setq tab-width 2)
    (define-key css-mode-map [return] 'newline-and-indent)
    (setq css-electric-brace-behavior t)
    (setq css-electric-semi-behavior t)
    (setq css-indent-offset 2)
    (setq css-indent-offset 2)))

(use-package sass-mode
  :config
  (progn
    (add-hook 'sass-mode-hook 'my-sass-comment-fix)
    (add-hook 'sass-mode-hook 'hexcolour-add-to-font-lock)
    ))

(use-package haml-mode
  :mode (("\\.haml$" . haml-mode))
  :config (setq haml-backspace-backdents-nesting nil))

(use-package slim-mode
  :mode (("\\.slim$" . slim-mode))
  :config ())
;;----------------------------------------------------------------------------
;; Buffer customizations
;;----------------------------------------------------------------------------

(use-package ibuffer
  :init
  (progn
    (autoload 'ibuffer "ibuffer" "List buffers." t)
    (define-key ibuffer-mode-map [delete] 'ignore)
    (define-key ibuffer-mode-map "j" 'evil-next-line)
    (define-key ibuffer-mode-map "k" 'evil-previous-line)
    (add-hook 'ibuffer-mode-hook
              '(lambda ()
                 (ibuffer-auto-mode 1)
                 (ibuffer-switch-to-saved-filter-groups "home")
                 (ibuffer-add-to-tmp-hide "^\\*")
                 (ibuffer-add-to-tmp-hide "^\\*Messages*")
                 (ibuffer-add-to-tmp-hide "^\\*Ibuffer*")
                 (ibuffer-add-to-tmp-hide "^\\*TAGS*")
                 (ibuffer-add-to-tmp-hide "^\\*scratch*")
                 (ibuffer-add-to-tmp-hide "^\\*Compile-Log*")
                 ))
    )
  :config
  (progn
    (setq ibuffer-expert t) ;; deltes buffers without asking
    (setq ibuffer-shrink-to-minimum-size t)
    (setq ibuffer-always-show-last-buffer nil)
    (setq ibuffer-sorting-mode 'recency)
    (setq ibuffer-use-header-line t)
    (setq ibuffer-auto-mode 1)
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 40 20) " " filename)
            ))
    ;; Categorize buffers
    (setq ibuffer-saved-filter-groups
          (quote (("home"
                   ("css" (mode . css-mode))
                   ("html" (mode . rhtml-mode))
                   ("haml" (mode . haml-mode))
                   ("sass" (mode . sass-mode))
                   ("coffee" (mode . coffee-mode))
                   ("javascript" (mode . js-mode))
                   ("markdown" (mode . markdown-mode))
                   ("yaml" (mode . yaml-mode))
                   ("lisp" (mode . emacs-lisp-mode))
                   ("rails views" (or (filename . "/app/views/")
                                      (name . "\\.erb$")
                                      (name . "\\.rhtml$")
                                      (name . "\\.haml$")
                                      (mode . rhtml-mode)))
                   ("helpers" (filename . "/app/helpers/.*\\.rb$"))
                   ("specs" (name . "_spec.rb$"))
                   ("models" (filename . "/app/models/.*\\rb$"))
                   ("controllers" (filename . "/app/controllers/.*\\.rb$"))
                   ("routes" (or (filename . "/configuroutes/")
                                 (name . "routes.rb$")
                                 (mode . rhtml-mode)))

                   ("ruby" (mode . ruby-mode))
                   ("console" (mode . term-mode))
                   ("terminals" (mode . term-mode))
                   ))))))

;;----------------------------------------------------------------------------
;; Shell customizations
;;----------------------------------------------------------------------------

(use-package rainbow-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-mode)
    ))

(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ))

;;----------------------------------------------------------------------------
;; Use Multiple Major Mode to highligh code snippets in Markdown files.
;;----------------------------------------------------------------------------

(use-package mmm-mode
  :init
  (progn
    (mmm-add-group
     'html-js
     '((js-script-cdata
        :submode js-mode
        :face mmm-code-submode-face
        :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
        :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
       (js-script
        :submode js-mode
        :face mmm-code-submode-face
        :front "<script[^>]*>[ \t]*\n?"
        :back "[ \t]*</script>"
        :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                     @ "" _ "" @ "\n</script>" @)))))
    (mmm-add-classes
     '((markdown-ruby
        :submode ruby-mode
        :face mmm-declaration-submode-face
        :front "^~~~ruby[\n\r]+"
        :back "^~~~$")))

    (mmm-add-classes
     '((markdown2-ruby
        :submode ruby-mode
        :face mmm-declaration-submode-face
        :front "^```ruby[\n\r]+"
        :back "^```$")))

    (mmm-add-classes
     '((markdown-haml
        :submode haml-mode
        :face mmm-declaration-submode-face
        :front "^```haml[\n\r]+"
        :back "^```$")))

    (mmm-add-classes
     '((markdown-elixir
        :submode elixir-mode
        :face mmm-declaration-submode-face
        :front "^~~~elixir[\n\r]+"
        :back "^~~~$")))

    (setq mmm-global-mode 't)
    (setq mmm-submode-decoration-level 0)
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-haml))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-ruby))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown2-ruby))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-elixir))
    ))

;;----------------------------------------------------------------------------
;; Code snippets
;;----------------------------------------------------------------------------
;; (use-package snippet
;;   :config
;;   (progn
;;     ))


;; (use-package yasnippet
;;   :init
;;   (progn
;;     (yas-global-mode 1)
;;     (setq yas-snippet-dirs
;;           '("~/.emacs.d/snippets"                 ;; personal snippets
;;             ))
;;     (yas/global-mode t)
;;     ))
(use-package ivy
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    :config
    (setq ivy-wrap t)
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    )
  )

(use-package fullframe
  :config
  (progn
    (fullframe magit-status magit-mode-quit-window)
    )
  )

(use-package powerline
  :init
  (progn
    (powerline-clean-theme)))
