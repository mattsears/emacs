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

;; (ac-set-trigger-key "TAB")
(custom-set-variables
 '(ac-trigger-key "TAB")
 '(ac-auto-start nil)
 '(ac-use-menu-map t))


(vendor 'yasnippet)
(require 'yasnippet)

;; (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/vendor/yasnippets")
(yas/load-directory "~/.emacs.d/vendor/cucumber/snippets/feature-mode")
;; (setq yas/trigger-key "TAB")

(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\t" 'ac-expand)

(provide 'snippets)

