
;; Debind the control-tab command
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
