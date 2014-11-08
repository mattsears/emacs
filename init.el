;;----------------------------------------------------------------------------
;; Global Package Initializations
;;----------------------------------------------------------------------------

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Loads $PATH when Emacs is opened via gui
(push "/usr/local/bin" exec-path)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Use-package loads packages only when packages are loaded
(require 'use-package)

(load "defuns")
(load "rails-edbi")

;;----------------------------------------------------------------------------
;; Global settings for all modes
;;----------------------------------------------------------------------------

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

;; Make yes/no options y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup settings
(setq auto-fill-mode 1)
(setq auto-save-mode nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Misc settings
(setq global-hl-line-mode t)
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

;; Bash
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))

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

;;----------------------------------------------------------------------------
;; Colors and UI improvements
;;----------------------------------------------------------------------------

(load-file "~/.emacs.d/color-theme-neptune.el")
(color-theme-neptune)

;; Set the default font-size to 16pt
(set-face-attribute 'default nil :height 160)

;; Modeline colors
(set-face-attribute 'mode-line nil
                    :foreground "#eeeeee"
                    :background "#222222"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "#7f7f7f"
                    :background "#222222"
                    :box nil)

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

;;----------------------------------------------------------------------------
;; Diff mode cosmetics
;;----------------------------------------------------------------------------
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "#9fab7d")
     (set-face-foreground 'diff-removed "#CF6A4C")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "#9fab7d")
     (set-face-foreground 'magit-diff-del "#CF6A4C")))

;;----------------------------------------------------------------------------
;; Change the default colors for matching parens
;;----------------------------------------------------------------------------

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;;------------------------------------------------------------------------------
;; Enhanced M-x
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
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-use-virtual-buffers t)
    (setq ido-ubiquitous-mode 1)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-file-extensions-order '(".rb" ".el" ".coffee" ".js"))
    (setq ido-save-directory-list-file nil)
    (setq ido-enable-flex-matching 1) ; fuzzy matching is a must have
    (setq ido-default-buffer-method (quote selected-window))
    (setq ido-default-file-method (quote selected-window))
    (setq ido-enable-tramp-completion t)
    (setq ido-use-filename-at-point t)
    (setq ido-use-url-at-point nil)
    (setq ido-max-prospects 10)
    (setq ido-confirm-unique-completion t)
    (setq ido-show-dot-for-dired t)
    (setq ido-work-directory-list '("~/" "~/Desktop" "~/Documents"))
    (setq ido-auto-merge-work-directories-length -1)
    (setq ido-use-faces 1)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; Flx Fuzzy file finder integration with IDO
(use-package flx-ido
  :init (flx-ido-mode 1))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1)
  :config
  (progn
    (setq ido-decorations (quote ("\n↪ "     "" "\n   " "\n   ..." "[" "]"
                                  " [No match]" " [Matched]" " [Not readable]"
                                  " [Too big]" " [Confirm]")))
    (setq ido-ignore-buffers
          '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*Ibuffer"
            "^\*scratch*" "^\*TAGS" "^session\.*" "^\*"))
    ))


;; Yet another paste tool, this one for Gist (awesome)
(use-package gist)

;; Keep a history of recent changes
(use-package recentf
  :init
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 60)))

;; Custom alignment rules
(use-package align
  :init
  (progn
    (add-to-list 'align-rules-list
                 '(text-column-whitespace
                   (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                   (group   . 2)
                   (modes   . align-text-modes)
                   (repeat  . t)))
    ))

;;----------------------------------------------------------------------------
;; Silver Searcher searching
;;----------------------------------------------------------------------------
(use-package ag
  :init
  (progn
    (setq ag-highlight-search t)))

;;----------------------------------------------------------------------------
;; Helpful hints for key bindings
;;----------------------------------------------------------------------------
(use-package guide-key
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-c" "C-x 4" "C-c r" "C-c p" "C-c r g" "C-c m"))
    (setq guide-key/popup-window-position 'bottom)
    (guide-key-mode 1)  ; Enable guide-key-mode
    ))

;;----------------------------------------------------------------------------
;; Snippets
;;----------------------------------------------------------------------------
;; (use-package yasnippet
;;   :init
;;   (progn
;;     (yas-global-mode 1)
;;     (setq-default yas/prompt-functions '(yas/ido-prompt))))

;;----------------------------------------------------------------------------
;; Projectile for project file navigation
;;----------------------------------------------------------------------------
(use-package projectile-rails)

(use-package projectile
  :bind ("s-t" . projectile-find-file)
  :init
  (projectile-global-mode)
  :config
  (progn
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    (setq projectile-cache-file (expand-file-name ".projectile.cache" user-emacs-directory))
    (setq projectile-known-projects-file (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

;; Yaml
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; Spell checker
(use-package flyspell
  :init
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-flyspell)))

;; (defun turn-on-flyspell ()
;;   "Force flyspell-mode on using a positive arg.  For use in hooks."
;;   (interactive)
;;   (flyspell-mode 1))

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
(use-package flycheck
  :init
  (progn
    (add-hook 'ruby-mode-hook 'flycheck-mode)
    (add-hook 'coffee-mode-hook 'flycheck-mode)
    (add-hook 'emacs-lisp 'flycheck-mode)
    (add-hook 'elixir 'flycheck-mode)
    (set-face-attribute 'flycheck-fringe-info nil :foreground "#7aa6da")
    (set-face-attribute 'flycheck-info nil :underline '(:style wave :color "#e28964"))
    (set-face-attribute 'flycheck-error nil :foreground "#fad07a" :weight 'bold :background nil)
    (set-face-attribute 'flycheck-warning nil :weight 'bold :underline "#cdc098" :foreground nil :background nil)
    ))

;;----------------------------------------------------------------------------
;; Autocomplete all the things
;;----------------------------------------------------------------------------

(defvar-local company-col-offset 0 "Horisontal tooltip offset.")
(defvar-local company-row-offset 0 "Vertical tooltip offset.")

(use-package company
  :init
  (progn
    (global-company-mode t)
    (setq company-tooltip-limit 12)                      ; bigger popup window
    (setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
    (setq company-echo-delay 0)                          ; remove annoying blinking
    (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
    (setq company-dabbrev-downcase nil)                  ; Do not convert to lowercase
    (setq company-selection-wrap-around t)               ; continue from top when reaching bottom
    ;; Hack to trigger candidate list on first TAB, then cycle through candiates with TAB
    (defvar tip-showing nil)
    (eval-after-load 'company
      '(progn
         (define-key company-active-map (kbd "TAB")  (lambda ()
                                                       (interactive)
                                                       (company-complete-common)
                                                       (if tip-showing
                                                           (company-select-next))
                                                       ))
         (define-key company-active-map [tab] 'company-select-next)))

    (setq company-frontends '(company-pseudo-tooltip-on-explicit-action company-echo-metadata-on-explicit-action-frontend company-preview-if-just-one-frontend))
    ))

;;----------------------------------------------------------------------------
;; Evil mode - uses Vim key commands
;;----------------------------------------------------------------------------
(use-package evil
  :init
  (progn
    (evil-mode 1)
    ;; Disable evil for certain major-modes
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-visual-state-map (kbd "C-]") 'text-shift-right)
    (define-key evil-visual-state-map (kbd "C-[") 'text-shift-left)
    (setq evil-want-fine-undo t)

    (defun set-mode-to-default-emacs (mode)
      (evil-set-initial-state mode 'emacs))

    (add-hook 'compilation-mode-hook '(lambda ()
                                        (local-unset-key "g")
                                        (local-unset-key "h")
                                        (local-unset-key "k")))

    (mapcar 'set-mode-to-default-emacs
            '(dired
              magit-branch-manager-mode
              comint-mode
              magit-log-mode
              eshell-mode
              diff-mode
              project-explorer-mode))

    (lexical-let ((default-color (cons (face-background 'mode-line)
                                       (face-foreground 'mode-line))))
      (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))
    ))


;; Make a visual selection with v or V, and then hit * to search that
;; selection forward, or # to search that selection backward.
(use-package evil-visualstar)

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

;; (use-package surround
;;   :init
;;   (global-surround-mode 1 ))

(use-package evil-nerd-commenter)

(use-package evil-leader
  :init
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "t"   'projectile-find-file
      "o"   'dired
      "g"   'magit-status
      "b"   'projectile-switch-to-buffer
      "i"   'iwb
      ","   'switch-to-previous-buffer
      "."   'projectile-find-tag
      "TAB" 'projectile-ibuffer
      "/"   'projectile-ag
      "w"   'matts-close-and-delete-window
      "x"   'smex
      "k"   'matts-close-and-delete-window
      "y"   'matts-ido-goto-symbol
      "c"   'evilnc-comment-or-uncomment-lines
      "R"   'matts-ido-choose-from-recentf

      "rm"  'projectile-rails-find-model
      "rc"  'projectile-rails-find-controller
      "rv"  'projectile-rails-find-view
      "rt"  'projectile-rails-find-spec
      "rh"  'projectile-rails-goto-schema
      "rr"  'projectile-rails-goto-routes
      "rg"  'projectile-rails-find-migration
      "rs"  'projectile-rails-find-stylesheet
      "rl"  'projectile-rails-find-lib

      "r.m" 'projectile-rails-find-current-model
      "r.t" 'projectile-rails-find-current-spec
      "r.g" 'projectile-rails-find-current-migration

      "rdb" 'my-rails-database

      "m"   'list-bookmarks
      "p"   'matts-ido-find-project
      "e"   'project-explorer-open)
    ))

;;----------------------------------------------------------------------------
;; Keychord allows you to assign key commands with keys press simultanously.
;;----------------------------------------------------------------------------
(use-package key-chord
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-define-global "hj" 'evil-normal-state)
    (key-chord-define-global "jk" 'evil-normal-state)))

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
    (local-set-key "\C-m" 'matts-dired-find-file)
    (local-set-key "^" 'matts-dired-up-directory)
    (define-key dired-mode-map [return] 'dired-find-alternate-file)
    ))

;;----------------------------------------------------------------------------
;; Markdown mode options
;;----------------------------------------------------------------------------
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         (".simplenote\\'" . markdown-mode)))

;;----------------------------------------------------------------------------
;; Clojure mode
;;----------------------------------------------------------------------------
(use-package clojure-mode)

;;----------------------------------------------------------------------------
;; Elixir
;;----------------------------------------------------------------------------
(defun elixir-mode-compile-on-save ()
  "Elixir mode compile files on save."
  (and (file-exists (buffer-file-name))
       (file-exists (elixir-mode-compiled-file-name))
       (elixir-cos-mode t)))

(use-package elixir-mode
  :init
  (progn
    (use-package elixir-mix)
    ;;(elixir-mode-compile-on-save)
    )
  :bind ("C-c , v" . elixir-mix-test-this-buffer))

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
         ("\\.mustache\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)
    (setq web-mode-comment-style 2)))

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
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2))))
  :config
  (setq js-indent-level 2))

;;----------------------------------------------------------------------------
;; Magit - awesome Git integration
;;----------------------------------------------------------------------------
(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (setq yas/dont-activate t)
    (set-face-background 'magit-item-highlight "#0C1021")
    (set-face-foreground 'magit-diff-add "#79b958")
    (set-face-foreground 'magit-diff-del "#d95c47"))
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil))
  :bind ("C-x g" . magit-status))

(use-package magit-gh-pulls
  :config
  (progn
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

;;----------------------------------------------------------------------------
;; Git Gutter
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
;; ERC mode
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
    (use-package inf-ruby
      :init
      (progn
        (inf-ruby-minor-mode)))
    (use-package rvm)
    (use-package rbenv
      :init
      (progn (global-rbenv-mode)))
    (use-package robe)
    (use-package ruby-hash-syntax)
    (use-package minitest)
    )
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rspec-mode)
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'ruby-mode-hook
              (lambda () (modify-syntax-entry ?_ "w")))
    (setq ruby-deep-indent-paren nil)
    (setq rspec-use-bundler-when-possible nil)
    (setq rspec-use-rake-when-possible nil)
    (setq ruby-deep-indent-paren-style nil)
    (setq ruby-deep-arglist nil)
    (setq ruby-dbg-flags "-W0")
    (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
    (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))
    )
  :bind (("C-{" . ruby-toggle-hash-syntax)
         ("C-M-h" . backward-kill-word)
         ("C-r" . ruby-compilation-this-buffer))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("\\.rabl$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
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
    (setq cssm-mirror-mode nil)
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
                   ("cucumber" (mode . feature-mode))
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
                   ("tests" (name . "_test.rb$"))
                   ("specs" (name . "_spec.rb$"))
                   ("models" (filename . "/app/models/.*\\rb$"))
                   ("controllers" (filename . "/app/controllers/.*\\.rb$"))
                   ("routes" (or (filename . "/config/routes/")
                                 (name . "routes.rb$")
                                 (mode . rhtml-mode)))

                   ("ruby" (mode . ruby-mode))
                   ("console" (mode . term-mode))
                   ("terminals" (mode . term-mode))
                   ))))))

;;----------------------------------------------------------------------------
;; Shell customizations
;;----------------------------------------------------------------------------

(use-package multi-term
  :init
  (progn
    (setq yas/dont-activate t)
    (defun my-term-use-utf8 ()
      (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
    (add-hook 'term-exec-hook 'my-term-use-utf8)
    (add-hook 'term-mode-hook (lambda()
                                (setq yas-dont-activate t)))
    (use-package bash-completion
      :init
      (progn
        (bash-completion-setup)
        )))
  :config
  (progn
    (setq multi-term-program "/usr/local/bin/bash")
    ))

(use-package writeroom-mode)
