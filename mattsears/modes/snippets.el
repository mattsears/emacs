;;----------------------------------------------------------------------------
;; Snippets
;;----------------------------------------------------------------------------

(vendor 'yasnippet)
(require 'yasnippet)

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (make-variable-buffer-local 'yas/trigger-key)
;;              (setq yas/trigger-key [tab])))

(yas/initialize)
(setq yas/text-popup-function
      'yas/dropdown-list-popup-for-template)

;; Load my custom snippets
(yas/load-directory "~/.emacs.d/mattsears/snippets")
(make-variable-buffer-local 'yas/trigger-key)
(setq yas/global-mode t)
