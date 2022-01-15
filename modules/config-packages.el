;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

;; Open files on github
(use-package browse-at-remote)

;; Smex - Enhanced M-x
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

;; Keep a history of recent changes
(use-package recentf
  :init
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 60)))

;; Silver Searcher searching (Say that 3 times in a row)
(use-package ag
  :init
  (progn
    (setq ag-highlight-search t)))

;; Projectile for project file navigation
(use-package projectile
  :defer 1
  :config
  (progn
    (projectile-global-mode)
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    (setq projectile-cache-file (expand-file-name ".projectile.cache" user-emacs-directory))
    (setq projectile-known-projects-file (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'selectrum-completing-read)
    (setq projectile-sort-order 'recently-active)
    (setq projectile-indexing-method 'alien)
    (setq projectile-switch-project-action 'neotree-projectile-action)

    (setq projectile-globally-ignored-files
          (append '(
                    ".DS_Store"
                    "*.gz"
                    "*.pyc"
                    "*.jar"
                    "*.tar.gz"
                    "*.tgz"
                    "*.zip"
                    )
                  projectile-globally-ignored-files))
    (setq projectile-globally-ignored-directories
          (append '(
                    ".git"
                    ".svn"
                    "node_modules"
                    "repl"
                    "target"
                    "venv"
                    "logs"
                    "tmp"
                    "coverage"
                    "dump"
                    )
                  projectile-globally-ignored-directories))

    )
  )

;; Adds projectile helpers functions for Rails projects
(use-package projectile-rails
  :defer 1)

;; Spell checker
(use-package flyspell
  :init
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-flyspell)
    (add-hook 'org-mode-mode-hook 'turn-on-flyspell)
    ))

(use-package flyspell
  :defer 1
  :delight
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-mode 1)
  :init
  (progn
    (add-hook 'markdown-mode-hook
              (lambda ()
                (flyspell-mode 1)))))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-word-generic))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))

(use-package ispell
  :custom
  (ispell-silently-savep t))

;;----------------------------------------------------------------------------
;; Indicate syntax errors
;;----------------------------------------------------------------------------

(use-package flycheck
  :ensure t
  :init
  (progn

    (global-flycheck-mode)
    (setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
    (setq flycheck-highlighting-mode (quote lines))
    (setq flycheck-indication-mode (quote right-fringe))

    (add-hook 'ruby-mode-hook 'flycheck-mode)
    (add-hook 'haml-mode-hook 'flycheck-mode)
    (setq-default flycheck-disabled-checkers '(chef-foodcritic))
    (setq flycheck-check-syntax-automatically '(mode-enabled save))

    (flycheck-def-config-file-var flycheck-haml-lintrc haml-lint ".haml-lint.yml"
      :safe #'stringp)

    (flycheck-define-checker haml-lint
      "A haml-lint syntax checker"
      :command ("~/.rbenv/shims/haml-lint"
                (config-file "--config" flycheck-haml-lintrc)
                source)
      :error-patterns
      ((warning line-start
                (file-name) ":" line " [W] "  (message)
                line-end))
      :modes (haml-mode))

    (add-to-list 'flycheck-checkers 'haml-lint)

    )
  )

;;----------------------------------------------------------------------------
;; Hulk smash keyboard (to run commands)
;;----------------------------------------------------------------------------

(use-package key-chord
  :defer t
  :init
  (progn
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-define evil-normal-state-map "tt" 'projectile-toggle-between-implementation-and-test)
    (key-chord-define-global "jk" 'my-save-if-bufferfilename)
    ))

;;----------------------------------------------------------------------------
;; Make copy and paste actually work
;;----------------------------------------------------------------------------

(use-package pbcopy
  :defer 1
  :init
  (progn
    (turn-on-pbcopy)
    ))

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
;; Code snippets
;;----------------------------------------------------------------------------

(use-package yasnippet
  :ensure t
  ;; :defer 1
  ;; :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (yas-global-mode)
  :init
  (progn
    (yas-global-mode 1)

    (setq yas-snippet-dirs
          '("~/.emacs.d/snippets"                 ;; personal snippets
            ))
    (yas/global-mode t)
    ))

(use-package counsel
  :after ivy
  :delight
  :config (counsel-mode)
  :init
  (progn
    ))

(use-package ivy
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    :config
    (setq ivy-wrap t)
    (setq ivy-display-style 'fancy)
    (setq ivy-on-del-error-function #'ignore)
    (setq ivy-initial-inputs-alist nil) ;; Removes ^ from M-x input
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    )
  )

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :init
  (progn
    (ivy-rich-mode 1)
    )
  )

;; Very handy for quickly jumping to method definitions.
(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy)
  )

(use-package neotree
  :init
  (progn
    (setq neo-window-width 40)
    (setq neo-autorefresh nil)
    ;; (setq neo-vc-integration t)
    ;; (setq neo-banner-message "🚀")
    (setq neo-show-slash-for-folder nil)
    (setq neo-show-updir-line nil)
    (setq neo-smart-open t)

    (setq neo-hidden-regexp-list '("\\.pyc$" "\\.elc$" ".tramp-autosave" "\\.save$" "\\.dat$"
                                   "\\.keep$" "node_modules" "coverage" "vendor" "bin" ".git" "log" "tmp" "var"
                                   ".cask" ".tramp-autosave" "elpa" "transient" "auto-save-list"
                                   ".bundle" ".yard" ".projectile" ".cache" "\\.DS_Store$"))

    (setq neo-theme (if (display-graphic-p) 'icons 'nerd))

    :config
    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "l") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "h") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "n") 'neotree-create-node)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
    )
  )

(use-package company
  :init
  (setq company-backends '(
                           company-capf
                           company-keywords
                           company-semantic
                           company-files
                           company-etags
                           company-elisp
                           company-yasnippet
                           ))

  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'global-company-mode-hook
              (lambda ()
                (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
                (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort))
              )
    )
  )


;; Provides the ability to have commands and their history saved so that
;; whenever you return to work, you can re-run things as you need them. This is
;; not a radical function, it is part of a good user experience.
(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length t)
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring))
  (savehist-file (expand-file-name "history" user-emacs-directory))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("●" "▲" "■" "✶" "◉" "○" "○")))

(use-package dash
  :ensure t)


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-diagnostics-scope :workspace)

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (ruby-mode . lsp)
         (js-mode . lsp)
         )
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)


(use-package selectrum
  :ensure t
  :init
  (progn
    (selectrum-mode +1)
    )
  )

(use-package popwin
  :ensure t
  :init
  (progn
    ;; (setq neo-window-width 50)
    (defun matts/compile-autoclose (buffer string)
      (cond ((string-match "finished" string)
             (message "Build maybe successful: closing window.")
             (run-with-timer 1 nil
                             'delete-window
                             (get-buffer-window buffer t)))
            (t (message "Compilation exited abnormally: %s" string))))
    (setq compilation-finish-functions 'matts/compile-autoclose)
    (popwin-mode 1)
    )
  )

;; Highlights columns (helpful with long ERB files)
(use-package highlight-indentation
  :ensure t
  :commands highlight-indentation-current-column-mode
  )

(provide 'config-packages)
