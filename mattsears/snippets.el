;;----------------------------------------------------------------------------
;; Snippets
;;----------------------------------------------------------------------------

(vendor 'yasnippet)
(require 'yasnippet)
(yas/initialize)

;; Load my custom snippets
(yas/load-directory "~/.emacs.d/mattsears/snippets")
(yas/load-directory "~/.emacs.d/vendor/yasnippets-shoulda")
(yas/load-directory "~/.emacs.d/vendor/cucumber/snippets/feature-mode")
(setq yas/global-mode t)


;;----------------------------------------------------------------------------
;; Smart-tab (https://github.com/genehack/smart-tab.git)
;;----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/vendor/smart-tab")
(require 'smart-tab)
(global-smart-tab-mode 1)