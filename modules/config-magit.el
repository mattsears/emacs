;;----------------------------------------------------------------------------
;; Magit - awesome Git integration
;;----------------------------------------------------------------------------

(use-package magit
  :defer 2
  :init
  (progn
    (use-package magit-blame)
    (setq yas/dont-activate t)

    ;;Evil XML Attributes Text Object
    (use-package exato :ensure t)

    ;; Forge for interfacing with Github
    (use-package forge :ensure t)

    ;; make magit status go full-screen but remember previous window
    ;; settings from: http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    ;; Close popup when commiting - this stops the commit window hanging around
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
    (add-hook 'magit-log-edit-mode-hook
              (lambda ()
                ;; highlight too-long commit summary
                (set (make-local-variable 'whitespace-line-column) 50)
                (set (make-local-variable 'whitespace-style) '(face lines-tail))
                (whitespace-mode 1)
                ;; autofill longer explanatory text
                (setq fill-column 72)
                (auto-fill-mode 1)))
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
        (magit-commit-create)))

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
    )
  :bind ("C-x g" . magit-status)
  )

(provide 'config-magit)
