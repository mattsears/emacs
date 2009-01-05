(require 'snippet)

;; Load yasnippet directory
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets/")
(set-face-background 'yas/field-highlight-face "#0C1021")

;; Make yas work with a hippie
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-complete-abbrev
	    try-complete-file-name
	    try-expand-dabbrev))