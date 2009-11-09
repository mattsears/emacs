(add-to-list 'load-path "~/.emacs.d/vendor")

; custom place to save customizations
(setq custom-file "~/.emacs.d/mattsears/custom.el")
(load custom-file)

(load "mattsears/global")
(load "mattsears/defuns")
(load "mattsears/bindings")
(load "mattsears/modes")
(load "mattsears/theme")
(load "mattsears/local")

(vendor 'cheat)
(vendor 'treetop)
(vendor 'css)
(vendor 'ack)
(vendor 'twit)
(vendor 'rdebug)

;; Autoloads
(autoload 'google-define "google-define" nil t)

(smex-initialize)
