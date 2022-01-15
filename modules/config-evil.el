;;----------------------------------------------------------------------------
;; Evil mode - uses modal editing key commands
;;----------------------------------------------------------------------------
(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-want-keybinding nil)

    (evil-mode t)
    (evil-collection-init)
    ;; Make a escape actually quit
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (global-set-key [escape] 'keyboard-quit)

    (define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)
    (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "C-n") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-window-up)
    (define-key evil-visual-state-map (kbd "C-]") 'matts/shift-right)
    (define-key evil-visual-state-map (kbd "C-[") 'matts/shift-left)

    (define-key evil-insert-state-map [remap newline] 'evil-ret-and-indent)

    (setq evil-want-fine-undo t)
    (setq evil-default-cursor t)
    (setq evil-normal-state-cursor '(box "orange"))
    (setq evil-insert-state-cursor '(bar "orange"))
    (setq evil-visual-state-cursor '(hollow "orange"))

    (unless (display-graphic-p)
      (require 'evil-terminal-cursor-changer)
      (evil-terminal-cursor-changer-activate) ; or (etcc-on)
      )

    ;; Changing/deleting/yanking text will not clobber your default clipboard.
    (setq x-select-enable-clipboard nil)

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

    ;; Change the modeline color based on evil mode
    (setq original-background (face-attribute 'mode-line :background))
    (setq normal-state-background "#646c9c")

    (add-hook 'evil-normal-state-entry-hook
              (lambda ()
                (set-face-attribute 'mode-line nil :background original-background)))
    (add-hook 'evil-normal-state-exit-hook
              (lambda ()
                (set-face-attribute 'mode-line nil :background normal-state-background)))
    (add-hook 'evil-insert-state-exit-hook (lambda() (message "insert exit hook") ))
    (add-hook 'evil-insert-state-entry-hook (lambda() (message "insert entry hook") ))

    (remove-hook 'evil-insert-state-exit-hook 'snippet-insert)
    (remove-hook 'evil-insert-state-entry-hook 'snippet-insert)
    (remove-hook 'evil-normal-state-exit-hook 'snippet-insert)
    (remove-hook 'evil-normal-state-entry-hook 'snippet-insert)
    (remove-hook 'evil-emacs-state-entry-hook 'snippet-insert)
    (remove-hook 'evil-emacs-state-exit-hook 'snippet-insert)
    (remove-hook 'evil-insert-state-exit-hook 'expand-abbrev)
    (remove-hook 'evil-insert-state-entry-hook 'expand-abbrev)

    )
  )

;; Make a visual selection with v or V, and then hit * to search forward
(use-package evil-visualstar
  :defer 2)

(use-package evil-surround
  :defer 2
  :init
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :defer 2
  :init
  (evil-commentary-mode))

(use-package evil-numbers
  :defer 2)

(use-package evil-rails
  :defer 2)

(use-package evil-magit
  :defer 2)

(use-package evil-lion
  :defer 2
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-escape
  :defer 2
  :init
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode 1))

(use-package evil-leader
  :init
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "t"   'counsel-projectile-find-file
      "o"   'dired-jump
      "b"   'counsel-projectile-switch-to-buffer
      "i"   'fwb
      ","   'switch-to-previous-buffer
      "/"   'counsel-projectile-ag
      "w"   'matts-close-and-delete-window
      "x"   'counsel-M-x
      "X"   'smex-major-mode-commands
      "k"   'matts-close-and-delete-window
      "s"   'matts-find-symbol
      "c"   'evil-commentary-line
      "R"   'matts-ido-choose-from-recentf
      "j"   'dumb-jump-go
      "p"   'dumb-jump-back
      "n"   'neotree-project-dir-toggle
      ;; "n"   'dired-sidebar-toggle-sidebar

      "g"  'hydra-magit/body'
      "f"  'hydra-files/body'
      "r"  'hydra-rails/body'
      "m"  'hydra-markdown/body'
      "l"  'hydra-org/body'

      "et"  'alchemist-mix-test
      "ec"  'alchemist-mix-compile
      "ex"  'alchemist-iex-project-run
      "er"  'alchemist-iex-project-run
      "ee"  'alchemist-eval-print-current-line
      ";"   'ispell-word
      )))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

(provide 'config-evil)
