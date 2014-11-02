;;----------------------------------------------------------------------------
;; Global Package Initializations
;;----------------------------------------------------------------------------

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Loads $PATH when Emacs is opened via gui
(push "/usr/local/bin" exec-path)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Use-package loads packages only when packages are loaded; thus
;; saving start-up time
(require 'use-package)

(load "defuns")
(load "settings")
(load "modes")
(load "key-bindings")
(load "appearance")
(load "rails-edbi")

;;------------------------------------------------------------------------------
;; Enhanced M-x
;;------------------------------------------------------------------------------
(use-package smex
  :init
  (progn
    (setq smex-save-file (dot-emacs ".smex.save"))
    (smex-initialize)))

;; A little smarter when hitting RET
(use-package smart-newline
  :init
  (progn (smart-newline-mode 1) ))

;; A little smarter parenthesis matching
(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (require 'smartparens-ruby)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (set-face-attribute 'sp-pair-overlay-face nil :background "#5f005f")
    (set-face-attribute 'sp-wrap-tag-overlay-face nil :background "#5f005f")
    (set-face-attribute 'sp-wrap-overlay-face nil :background "#5f005f")

    ))
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
;; Nice calendar
;;----------------------------------------------------------------------------
(use-package calfw)
(use-package calfw-ical)

;;----------------------------------------------------------------------------
;; Helpful hints for key bindings
;;----------------------------------------------------------------------------
(use-package guide-key
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-c" "C-x 4" "C-c r" "C-c p" "C-c r g" "C-c m"))
    (setq guide-key/popup-window-position 'bottom)
    (guide-key-mode 1)  ; Enable guide-key-mode
    (setq projectile-tags-command "ctags -Re %s --exclude=*.html,*.js")
    ))

;; Fixes some of Emacs undo/redo weirdness
(use-package redo+)

;; (require 'popup)
(use-package popwin
  :config (setq display-buffer-function 'popwin:display-buffer))

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

;; Highlight matching paren
;; (use-package paren
;;   :init
;;   (progn
;;     (set-face-background 'show-paren-match-face "#c397d8")
;;     (set-face-foreground 'show-paren-match "#def")
;;     (setq show-paren-delay 0)
;;     (setq show-paren-mode 1)
;;     (setq show-paren-style 'parenthesis)
;;     (show-paren-mode +1)))

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
    ;;(set-face-attribute 'flycheck-error-list-highlight-at-point nil :background "grey15")
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

    (defun company-pseudo-tooltip-on-explicit-action (command)
      "`company-pseudo-tooltip-frontend', but only on an explicit action."
      (when (company-explicit-action-p)
        (setq tip-showing t)
        (company-pseudo-tooltip-frontend command)))

    (defun company-echo-metadata-on-explicit-action-frontend (command)
      "`company-mode' front-end showing the documentation in the echo area."
      (pcase command
        (`post-command (when (company-explicit-action-p)
                         (company-echo-show-when-idle 'company-fetch-metadata)))
        (`hide
         (company-echo-hide)
         (setq tip-showing nil)
         )))

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
    ;; (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
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

    ;; (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    ;;   (recenter))
    ;; (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
    ;;     (recenter))

    ;; Make a visual selection with v or V, and then hit * to search that
    ;; selection forward, or # to search that selection backward.
    (use-package evil-visualstar)

    (use-package evil-matchit
      :init
      (global-evil-matchit-mode 1))

    (use-package surround
      :init
      (global-surround-mode 1 ))
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
          "rh"  'projectile-rails-goto-schema
          "rr"  'projectile-rails-goto-routes
          "rg"  'projectile-rails-find-migration
          "rs"  'projectile-rails-find-stylesheet
          "rs"  'projectile-rails-find-lib
          "r.m" 'projectile-rails-find-current-model
          "rdb" 'my-rails-database
          "m"   'list-bookmarks
          "p"   'matts-ido-find-project
          "e"   'project-explorer-open)
        ))
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
