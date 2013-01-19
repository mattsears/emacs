(add-to-list 'load-path "~/.emacs.d/vendor")

;; Loat Aquamacs customizations
(setq custom-file "~/.emacs.d/mattsears/custom.el")
(load custom-file)

(load "mattsears/global")
(load "mattsears/bindings")
(load "mattsears/defuns")
(load "mattsears/modes")
(load "mattsears/theme")
(load "mattsears/local")
(load "mattsears/snippets")

(vendor 'treetop)
(vendor 'css)
(vendor 'full-ack)
(vendor 'open-file-in-github)
(smex-initialize)

