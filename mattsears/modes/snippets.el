;; Load yasnippet directory
;;(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet-bundle")
;;(require 'yasnippet) ;; not yasnippet-bundle

;;(require 'yasnippet-bundle) ;; not yasnippet
;;(yas/initialize)
;;(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets/")

(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")

(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))
