;;----------------------------------------------------------------------------
;; Initialize individual mode customizations
;;----------------------------------------------------------------------------

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
;; Ruby mode
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
    (use-package feature-mode)
    (use-package sass-mode
      :config
      (progn (add-hook 'sass-mode-hook 'my-sass-comment-fix)))
    (use-package haml-mode
      :mode (("\\.haml$" . haml-mode))
      :config (setq haml-backspace-backdents-nesting nil))
    (use-package minitest)
    (use-package slim-mode
      :mode (("\\.slim$" . slim-mode)))
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
;; Css mode
;;----------------------------------------------------------------------------

;; Highlights HTML/CSS color specifications
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
     (0 (let ((colour (match-string-no-properties 0)))
          (if (or (= (length colour) 4)
                  (= (length colour) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)
                           :foreground (if (>= (apply '+ (x-color-values
                                                          (match-string-no-properties 0)))
                                               (* (apply '+ (x-color-values "white")) .6))
                                           "black" ;; light bg, dark text
                                         "white" ;; dark bg, light text
                                         )))))
        append))))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords t))

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

(add-hook 'sass-mode-hook
          'hexcolour-add-to-font-lock)

;;----------------------------------------------------------------------------
;; Org mode
;;----------------------------------------------------------------------------
(use-package org
  :config
  (progn
    ;; Debind the control-tab command
    (add-hook 'org-mode-hook
              '(lambda ()
                 (define-key org-mode-map [(control tab)] nil)))
    (setq org-log-done nil)
    (setq org-startup-indented nil)
    (setq org-todo-keywords
          '((sequence "TODO(t)" "IN-PROGRESS(p@/!)" "NEXT(n@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
    (custom-set-variables
     '(org-agenda-files (quote ("~/todos.org")))
     '(org-default-notes-file "~/notes.org")
     '(org-agenda-ndays 7)
     '(org-deadline-warning-days 14)
     '(org-agenda-show-all-dates t)
     '(org-agenda-skip-deadline-if-done t)
     '(org-agenda-skip-scheduled-if-done t)
     '(org-agenda-start-on-weekday nil)
     '(org-reverse-note-order t)
     '(org-fast-tag-selection-single-key (quote expert))
     '(org-agenda-custom-commands
       (quote (("d" todo "DELEGATED" nil)
               ("c" todo "DONE|DEFERRED|CANCELLED" nil)
               ("w" todo "WAITING" nil)
               ("W" agenda "" ((org-agenda-ndays 21)))
               ("A" agenda ""
                ((org-agenda-skip-function
                  (lambda nil
                    (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
                 (org-agenda-ndays 1)
                 (org-agenda-overriding-header "Today's Priority #A tasks: ")))
               ("u" alltodo ""
                ((org-agenda-skip-function
                  (lambda nil
                    (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                              (quote regexp) "\n]+>")))
                 (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
     '(org-remember-store-without-prompt t)
     '(remember-annotation-functions (quote (org-remember-annotation)))
     '(remember-handler-functions (quote (org-remember-handler))))
    ))

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
                   ("channels" (mode . erc-mode))
                   ("css" (mode . css-mode))
                   ("html" (mode . rhtml-mode))
                   ("handlebars" (or
                                  (name . "^\\*.jst.hbs.haml\\*$")
                                  (name . "^\\*.jst.hbs\\*$")))
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
    ;; (use-package ansi-term
    ;;   :init
    ;;   (progn
    ;;     (ad-activate 'ansi-term)))

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

(provide 'modes)
