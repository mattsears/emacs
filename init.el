(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))

;; custom place to save customizations
(setq custom-file "~/.emacs.d/mattsears/custom.el")
(load custom-file 'noerror)
 
(load "mattsears/global")
(load "mattsears/defuns")
(load "mattsears/bindings")
(load "mattsears/modes")
(load "mattsears/theme")
(load "mattsears/shell")
(load "mattsears/local") 

(vendor 'cheat)
(vendor 'treetop)
(vendor 'emac-rails)
(vendor 'css)
(vendor 'ack)
(vendor 'twit)
(vendor 'tail)