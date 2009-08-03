(add-to-list 'load-path "~/.emacs.d/vendor/mmm-mode/")
(add-to-list 'load-path "~/.emacs.d/vendor/nxml/")

;; nxml-mode & rhtml
(require 'nxml-mode)

;; Global MMM mode settings
(require 'mmm-mode)
(require 'mmm-auto)

(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(setq nxml-child-indent 2)

;; XML Setup
(load "~/.emacs.d/vendor/nxml/rng-auto.el")
(load-library "rng-auto")
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch"
                                               "rng" "xslt" "svg" "rss") t) "\\'") 'nxml-mode))
;; Nxml hooks
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (define-key nxml-mode-map "\M-h" 'backward-kill-word)
            (define-key nxml-mode-map "\C-m" 'newline-and-indent)
            (rng-validate-mode 0)
            (longlines-mode)
            (mmm-mode-on)
            (yas/minor-mode-on)
            (setq local-abbrev-table nxml-mode-abbrev-table)
            (message "My nxml-mode customizations loaded"))t)

(defvar nxml-mode-abbrev-table
  (let (table)
    (define-abbrev-table 'table ())
    table)
  "Abbrev table in use in ruby-mode buffers.")

(add-hook 'nxml-mode-hook 'llasram/nxml-set-abbrev-table)
(defun llasram/nxml-set-abbrev-table ()
  (setq local-abbrev-table nxml-mode-abbrev-table))

;; Add spell checking to the text between the nodes
(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

;; Destroy!
(setq magic-mode-alist
      '(("%![^V]" . ps-mode)
        ("# xmcd " . conf-unix-mode)))

;; Key bindings
(add-hook 'nxml-mode-hook 'llasram/nxml-extra-keys)
(defun llasram/nxml-extra-keys ()
  (define-key nxml-mode-map "\M-h" 'backward-kill-word)
  (define-key nxml-mode-map "\C-m" 'newline-and-indent))

(defun nxml-fontify-mode ()
  (nxml-mode)
  (mmm-mode-on)
  (font-lock-fontify-buffer)
  (nxml-fontify-buffer))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist (cons '("<＼＼?xml " . nxml-mode) magic-mode-alist))
(fset 'html-mode 'nxml-mode)
(fset 'html-helper 'nxml-mode)
(fset 'xml-mode 'nxml-mode)

;; MMM class for working with erb
(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "-?%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))

;; Add erb foo to HTML files
(mmm-add-mode-ext-class 'html-mode nil 'erb-code)
(mmm-add-mode-ext-class 'nxml-mode nil 'erb-code)

(setq auto-mode-alist (cons '("\\.html.erb$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . nxml-mode) auto-mode-alist))

(provide 'nxml)
