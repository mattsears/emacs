(load "~/.emacs.d/mattsears/modes/css")
(load "~/.emacs.d/mattsears/modes/diff")
(load "~/.emacs.d/mattsears/modes/dired")
(load "~/.emacs.d/mattsears/modes/erlang")
(load "~/.emacs.d/mattsears/modes/javascript")
(load "~/.emacs.d/mattsears/modes/ruby")
(load "~/.emacs.d/mattsears/modes/nxml")
(load "~/.emacs.d/mattsears/modes/ido")
(load "~/.emacs.d/mattsears/modes/snippets")

;; Misc Modes

; Bash
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))

;; Indicate syntax errors
(add-to-list 'load-path "~/.emacs.d/vendor/flymake.el")
(require 'flymake)

;; Minibuffer input completion and cycling.
(add-to-list 'load-path "~/.emacs.d/vendor/icicles")
(require 'icicles)

;; Yaml
(add-to-list 'load-path "~/.emacs.d/vendor/yaml")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Git goodness
(require 'magit)

;; Growl
(require 'growl)

;; Defunkt' Textmate
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; For automatic ()s
(add-to-list 'load-path "~/.emacs.d/vendor/paraedit.el")
(require 'paredit)

;; Midnight mode to clean up old buffers
(require 'midnight)

(provide 'modes)
