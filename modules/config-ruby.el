;;----------------------------------------------------------------------------
;; Ruby and related  modes
;;----------------------------------------------------------------------------

(use-package ruby-mode
  :init
  (progn
    ;; Removes the pesky issue with evil esacape and inserting a snippet for some reason
    (remove-hook 'ruby-mode-hook 'snippet-insert)

    (use-package ruby-tools
      :ensure t)

    (use-package rubocop
      :ensure t)

    (use-package inf-ruby
      :init
      (progn
        (inf-ruby-minor-mode)))

    ;; Auto detects the ruby verion for projects using Rbenv
    (use-package rbenv
      :init
      (progn (global-rbenv-mode)))

    (use-package ruby-block
      :ensure nil
      :config
      (progn
        (ruby-block-mode t)
        (setq ruby-block-delay 0.25)
        (setq ruby-block-highlight-toggle 'overlay)
        )
      )

    ;; Provides helpers for converting hashes to the newer syntax (eg {key: value})
    (use-package ruby-hash-syntax)
    )

  :config
  (progn
    (add-hook 'ruby-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
    (add-hook 'ruby-mode-hook #'rubocop-mode)

    (setq ruby-deep-arglist nil)
    (setq ruby-dbg-flags "-W0")
    (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
    (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

    ;; Goddammit ruby indention
    (setq ruby-align-to-stmt-keywords '(begin if while unless until case for def))
    (setq ruby-deep-indent-paren nil)
    (setq ruby-deep-indent-paren-style nil)
    (setq ruby-align-chained-calls nil)

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
    )
  :bind (("C-{" . ruby-toggle-hash-syntax))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("\\.decorator$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Berkshelf$" . ruby-mode)
         ("Procfile$" . ruby-mode)
         ("Procfile-dev$" . ruby-mode)
         ("Gemfile.lock$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

(provide 'config-ruby)
