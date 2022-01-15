
;;----------------------------------------------------------------------------
;; Use Multiple Major Mode to highligh code snippets in Markdown files.
;;----------------------------------------------------------------------------

(use-package mmm-mode
  :init
  (progn
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
     '((markdown-shell-script
        :submode shell-script-mode
        :face mmm-declaration-submode-face
        :front "^```shell[\n\r]+"
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

    (mmm-add-classes
     '((markdown-js
        :submode js-mode
        :face mmm-declaration-submode-face
        :front "^~~~javascript[\n\r]+"
        :back "^~~~$")))

    (mmm-add-classes
     '((markdown-erb
        :submode web-mode
        :face mmm-declaration-submode-face
        :front "^~~~erb[\n\r]+"
        :back "^~~~$")))

    (mmm-add-classes
     '((markdown-scss
        :submode web-mode
        :face mmm-declaration-submode-face
        :front "^~~~scss[\n\r]+"
        :back "^~~~$")))

    (require 'mmm-auto)
    (setq mmm-global-mode 'auto)
    (setq mmm-submode-decoration-level 0)

    (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
    (mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)

    (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))

    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-erb))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-scss))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-js))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-haml))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-ruby))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown2-ruby))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-elixir))
    (add-to-list 'mmm-mode-ext-classes-alist '(markdown-mode nil markdown-shell-script))

    ))

;;----------------------------------------------------------------------------
;; Markdown mode options
;;----------------------------------------------------------------------------

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         (".simplenote$" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode))

(use-package adoc-mode
  :mode (("\\.adoc$" . adoc-mode)))

;;----------------------------------------------------------------------------
;; Elixir and related modes
;;----------------------------------------------------------------------------

;;(use-package elixir-mode
;;  :init
;;  (progn
;;    (use-package alchemist)
;;    ))

;;----------------------------------------------------------------------------
;; Dired (Directory Edit) mode
;;----------------------------------------------------------------------------

(use-package dired
  :init
  :config
  (progn
    (use-package wdired)
    (use-package dired-x)

    (use-package dired-sidebar
      :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
      :ensure t
      :commands (dired-sidebar-toggle-sidebar)
      :init
      (add-hook 'dired-sidebar-mode-hook
                (lambda ()
                  (unless (file-remote-p default-directory)
                    (auto-revert-mode))))
      :config
      (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
      (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

      ;; (setq dired-sidebar-subtree-line-prefix "_")
      (setq dired-sidebar-theme 'nerd)
      (setq dired-sidebar-use-term-integration t)
      (setq dired-sidebar-use-custom-font t)

      (add-hook 'dired-sidebar-mode-hook
                (lambda ()
                  ;; (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                  (define-key evil-normal-state-local-map (kbd "l") 'dired-sidebar-subtree-toggle)
                  (define-key evil-normal-state-local-map (kbd "h") 'dired-sidebar-subtree-toggle)
                  (define-key evil-normal-state-local-map (kbd "RET") 'dired-sidebar-find-file)
                  ;; (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                  ;; (define-key evil-normal-state-local-map (kbd "n") 'neotree-create-node)
                  ;; (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                  ;; (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                  ))
      )

    (setq dired-omit-files
          (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                  (seq "~" eol)                 ;; backup-files
                  (seq "coverage" eol)          ;; code coverage files
                  )))

    ;; (setq-default dired-details-hidden-string "--- ")
    (setq dired-omit-extensions
          (append dired-latex-unclean-extensions
                  dired-bibtex-unclean-extensions
                  dired-texinfo-unclean-extensions))
    ;; (setq dired-details-hidden-string "[ ... ] ")

    (setq dired-listing-switches "-la") ;; How files are listed.
    (setq directory-free-space-args "-kh")
    (setq dired-omit-size-limit nil)
    (setq dired-dwim-target t)
    (setq dired-recursive-deletes 'top)
    (setq dired-recursive-copies (quote always))

    (add-hook 'dired-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "RET") 'dired-find-alternate-file)
                (define-key evil-normal-state-local-map (kbd "DEL") 'matts-dired-up-directory)
                (define-key evil-normal-state-local-map (kbd "o") 'matts-dired-open-mac)
                (define-key evil-normal-state-local-map (kbd "n") 'dired-touch-now)
                (define-key evil-normal-state-local-map (kbd "r") 'wdired-change-to-wdired-mode)

                ))

    ;; (define-key dired-mode-map [delete] 'dired-do-delete)
    ;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    ;; (define-key dired-mode-map (kbd "M-RET") 'dired-find-alternate-file)
    ;; (define-key dired-mode-map (kbd "DEL") 'matts-dired-up-directory)
    ;; (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
    ;; (define-key dired-mode-map "n" 'dired-touch-now)
    ;; (define-key dired-mode-map "o" 'matts-dired-open-mac)
    ;; (define-key dired-mode-map "a" 'dired-find-alternate-file)

    ))


;; Yaml
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; Crystal
(use-package crystal-mode)

;; Pug
(use-package pug-mode)

;; CSV mode
(use-package csv-mode)

;; Docker modes
(use-package dockerfile-mode)
(use-package docker-compose-mode)

(provide 'config-modes)
