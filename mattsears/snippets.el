;;----------------------------------------------------------------------------
;; Snippets
;;----------------------------------------------------------------------------

;; Autocomplete mode
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/ac-dict")
(ac-config-default)
(setq ac-auto-start nil)
(global-auto-complete-mode t)

(ac-set-trigger-key "TAB")
(custom-set-variables
 '(ac-trigger-key "TAB")
 '(ac-auto-start nil)
 '(ac-use-menu-map t))

(require 'yasnippet)
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/vendor/yasnippets")

(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\t" 'ac-expand)

(provide 'snippets)
