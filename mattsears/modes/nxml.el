(add-to-list 'load-path "~/.emacs.d/vendor/mmm-mode/")
(add-to-list 'load-path "~/.emacs.d/vendor/nxml/")

;; nxml-mode & rhtml
(require 'nxml-mode)

;; Global MMM mode settings
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(require 'mmm-sample)

(defvar nxml-mode-abbrev-table
  (let (table)
    (define-abbrev-table 'table ())
    table)
  "Abbrev table in use in ruby-mode buffers.")

(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

(defun nxml-fontify-mode ()
  (nxml-mode)
  (mmm-mode-on)
  (font-lock-fontify-buffer)
  (nxml-fontify-buffer))

;; Destroy!
;(setq magic-mode-alist
;      '(("%![^V]" . ps-mode)
;        ("# xmcd " . conf-unix-mode)))

;; nxml hooks
(add-hook 'nxml-mode-hook
       (lambda ()
         ;(define-key nxml-mode-map [return] 'newline-and-indent)
         (setq indent-tabs-mode nil)
		 (setq tab-width 4)
		 (define-key nxml-mode-map "\M-h" 'backward-kill-word)
	  	 (define-key nxml-mode-map "\C-m" 'newline-and-indent)
         (setq nxml-indent-offset 4)
		 (longlines-mode)
         (setq local-abbrev-table nxml-mode-abbrev-table)
         (message "My nxml-mode customizations loaded")))
						
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

(setq auto-mode-alist (cons '("\\.html.erb$" . nxml-mode) auto-mode-alist))

;; Add erb foo to HTML files
(mmm-add-mode-ext-class 'html-mode nil 'erb-code)
(mmm-add-mode-ext-class 'nxml-mode nil 'erb-code)

;; And YAML files while we're at it
(mmm-add-mode-ext-class 'yaml-mode nil 'erb-code)

(provide 'rhtml)
