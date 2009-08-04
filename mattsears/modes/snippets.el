;; Load yasnippet
(vendor 'yasnippet)
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
;; Load my custom snippets
(yas/load-directory "~/.emacs.d/mattsears/snippets")

;(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))
