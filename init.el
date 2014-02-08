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
(load "appearance")
(load "key-bindings")
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

;; Highlights each paren in various colors
(use-package rainbow-delimiters
  :init
  (progn
    (global-rainbow-delimiters-mode)
    ))

;;----------------------------------------------------------------------------
;; Project file search
;;----------------------------------------------------------------------------
(use-package full-ack
  :init
  (progn
    (autoload 'ack-same "full-ack" nil t)
    (autoload 'ack "full-ack" nil t)
    (autoload 'ack-find-same-file "full-ack" nil t)
    (autoload 'ack-find-file "full-ack" nil t)
    (setq ack-search-regexp nil)
    (setq ack-display-buffer t)))

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
(use-package yasnippet
  :init
  (progn
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

;;----------------------------------------------------------------------------
;; Smarter parenthesis matching
;;----------------------------------------------------------------------------
(use-package smartparens
  :init
  (progn
    (smartparens-global-mode 1)
    (sp-pair "'" nil :actions :rem)
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      )))

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
(use-package paren
  :init
  (progn
    (set-face-background 'show-paren-match-face "#373c4e")
    (setq show-paren-delay 0)
    (setq show-paren-mode 1)
    (setq show-paren-style 'parenthesis)
    (show-paren-mode +1)))

;; Yaml
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; Spell checker
(use-package flyspell
  :init
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-flyspell)))

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

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

;; Simplenotes
(use-package simplenote)

;;----------------------------------------------------------------------------
;; Graphically indicates the 80 column limit
;;----------------------------------------------------------------------------
(use-package fill-column-indicator
  :init
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "#1a1d2f")
    (add-hook 'emacs-lisp-mode-hook 'fci-mode)
    (add-hook 'ruby-mode-hook 'fci-mode)
    (add-hook 'haml-mode-hook 'fci-mode)
    (add-hook 'markdown-mode-hook 'fci-mode)
    (add-hook 'coffee-mode-hook 'fci-mode)
    (add-hook 'js-mode-hook 'fci-mode)
    (add-hook 'text-mode-hook 'fci-mode)
    (setq fill-column 80)))

;;----------------------------------------------------------------------------
;; Indicate syntax errors
;;----------------------------------------------------------------------------
(use-package flycheck
  :init
  (progn
    (add-hook 'ruby-mode-hook 'flycheck-mode)
    (add-hook 'coffee-mode-hook 'flycheck-mode)
    (add-hook 'sass-mode-hook 'flycheck-mode)
    (add-hook 'haml-mode-hook 'flycheck-mode)
    (add-hook 'emacs-lisp 'flycheck-mode)
    (add-hook 'elixir 'flycheck-mode)
    (add-hook 'sass 'flycheck-mode)
    (add-hook 'scss 'flycheck-mode)
    (set-face-attribute 'flycheck-fringe-info nil :foreground "#7aa6da")
    (set-face-attribute 'flycheck-info nil :underline '(:style wave :color "#e28964"))
    (set-face-attribute 'flycheck-error nil :foreground "#fad07a" :weight 'bold :background nil)
    (set-face-attribute 'flycheck-warning nil :weight 'bold :underline "#cdc098" :foreground nil :background nil)
    (set-face-attribute 'flycheck-error-list-highlight-at-point nil :background "grey15")))

;;----------------------------------------------------------------------------
;; Autocomplete all the things
;;----------------------------------------------------------------------------
(use-package auto-complete
  :init
  (progn
    (require 'auto-complete-config)
    (define-key ac-completing-map "RET" 'ac-complete)
    (setq ac-set-trigger-key "RET")
    ;; (setq ac-trigger-key "TAB")
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories
                 (dot-emacs ".cask/24.3.1/elpa/auto-complete-20131128.233/dict"))
    (setq ac-comphist-file (expand-file-name ".ac-comphist.dat" user-emacs-directory))
    (setq ac-ignore-case nil)
    (setq ac-disable-inline t)
    (setq ac-use-menu-map t)
    (add-to-list 'ac-modes 'ruby-mode)
    (add-to-list 'ac-modes 'web-mode)
    ))



(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))
(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

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
    (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
    (my-evil-terminal-cursor-change)
    (add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
    (use-package evil-matchit
      :init
      (global-evil-matchit-mode 1))
    (use-package surround
      :init
      (global-surround-mode 1 ))
    (use-package evil-leader
      :init
      (progn
        (global-evil-leader-mode)
        (evil-leader/set-leader ",")
        (evil-leader/set-key
          "t"   'projectile-find-file
          "o"   'dired
          "g"   'magit-status
          "b"   'switch-to-buffer
          ","   'switch-to-other-buffer
          "TAB" 'ibuffer
          "/"   'projectile-ag
          "w"   'matts-close-and-delete-window
          "x"   'smex
          "k"   'ido-bookmarks
          "y"   'ido-goto-symbol
          "c"   'comment-or-uncomment-line-or-region
          "rm"  'projectile-rails-find-model
          "rc"  'projectile-rails-find-controller
          "rv"  'projectile-rails-find-view
          "p"   'matts-ido-find-project)
        ))
    ))

;;----------------------------------------------------------------------------
;; Cool highlighting effect as you type
;;----------------------------------------------------------------------------
(use-package highlight-tail
  :init
  (progn
    (highlight-tail-mode)
    (setq highlight-tail-colors '(("black" . 0)
                                  ("#ec527a" . 25)
                                  ("black" . 66)))))

;;----------------------------------------------------------------------------
;; Keychord allows you to assign key commands with keys press simultanously.
;;----------------------------------------------------------------------------
(use-package key-chord
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "jk" 'evil-normal-state)))
