(add-to-list 'load-path "~/.emacs.d/vendor/mmm-mode/")
(add-to-list 'load-path "~/.emacs.d/vendor/nxml/")

;; nxml-mode & rhtml
(require 'nxml-mode)

;; Global MMM mode settings
(require 'mmm-mode)
(require 'mmm-auto)
(require 'mmm-sample)

(setq mmm-submode-decoration-level 2)
(setq mmm-global-mode 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/vendor/nxml/rng-auto.el")
(load-library "rng-auto")
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch"
                                               "rng" "xslt" "svg" "rss") t) "\\'") 'nxml-mode))
;; nxml hooks
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (define-key nxml-mode-map "\M-h" 'backward-kill-word)
            (define-key nxml-mode-map "\C-m" 'newline-and-indent)
            (setq nxml-indent-offset 4)
            (longlines-mode)
            (setq local-abbrev-table nxml-mode-abbrev-table)
            (message "My nxml-mode customizations loaded")))

(defvar nxml-mode-abbrev-table
  (let (table)
    (define-abbrev-table 'table ())
    table)
  "Abbrev table in use in ruby-mode buffers.")

;; Add spell checking to the text between the nodes
(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

(defun nxml-fontify-mode ()
  (nxml-mode)
  (mmm-mode-on)
  (font-lock-fontify-buffer)
  (nxml-fontify-buffer))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist (cons '("<＼＼?xml " . nxml-mode) magic-mode-alist))
(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)

;;------------------------------------------------------------------------- ---
;; Ruby - ERB
;;------------------------------------------------------------------------- ---

(mmm-add-group
 'fancy-html
 '((html-erb
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )
   (html-php-embeded
    :submode php-mode
    :face mmm-code-submode-face
    :front "<\\?\\(\\|php\\|=\\)?"
    :back "\\?>"
    :insert ((?p php-code   nil @ "<?php"  @ " " _ " " @ "?>" @)
             (?P php-print  nil @ "<?=" @ " " _ " " @ "?>" @))
    )
   (html-js-embeded
    :submode javascript-generic-mode
    :face mmm-code-submode-face
    :front "<script[^>]*type=\"text/javascript\"[^>]*"
    :back "</script>"
    )
   (html-js-attribute
    :submode javascript-generic-mode
    :face mmm-declaration-submode-face
    :front "\\bon\\w+=\""
    :back "\""
    )
   (html-css-embeded
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*type=\"text/css\"[^>]*>"
    :back "</style>"
    )
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "\\bstyle=\\s-*\""
    :back "\""
    )
   ))

(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil fancy-html))

(setq auto-mode-alist (cons '("\\.html.erb$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . nxml-mode) auto-mode-alist))
(add-hook 'nxml-mode-hook (lambda () (rinari-launch)))

;; Add erb foo to HTML, NXML, Yaml files
;;(mmm-add-mode-ext-class 'html-mode nil 'erb-code)
(mmm-add-mode-ext-class 'nxml-mode nil 'erb-code)
;;(mmm-add-mode-ext-class 'yaml-mode nil 'erb-code)

(provide 'rhtml)
