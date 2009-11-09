;;----------------------------------------------------------------------------
;; Auto Complete
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(when (require 'auto-complete nil t)
  ;;(require 'auto-complete-yasnippet)
  (require 'auto-complete-ruby)
  (require 'auto-complete-css)

  (global-auto-complete-mode t)         ;enable global-mode
  (setq ac-auto-start t)                ;automatically start
  (setq ac-dwim 3)                      ;Do what i mean
  (setq ac-override-local-map nil)      ;don't override local map
  ;;(define-key ac-complete-mode-map "\t" 'ac-expand)
  ;;(define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (define-key ac-complete-mode-map [down] 'ac-next)
  (define-key ac-complete-mode-map [up] 'ac-previous)
  (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

  (setq ac-modes
        (append ac-modes
                '(eshell-mode
                                        ;org-mode
                  )))
                                        ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq ac-sources '(ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq ac-sources '(ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))))

  (add-hook 'css-mode-hook
            (lambda ()
              (make-local-variable 'ac-sources)
              (setq ac-sources '(ac-source-css-keywords
                                 ac-source-words-in-buffer
                                 )))))
(provide 'autocomplete)
