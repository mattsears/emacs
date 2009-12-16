;; Prevent html-helper from hijacking html files
(add-to-list
 'magic-mode-alist
 '("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0" . rhtml-mode))

;;----------------------------------------------------------------------------
;; Ruby - RHTML Mode
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml/")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda ()
            (setq wrap-region-tag-active t)
            (define-key rhtml-mode-map "\C-RET" 'zencoding-expand-line)
            ;;(wrap-region-mode t)
            ))
(setq auto-mode-alist (cons '("\\.html\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mustache$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.liquidl$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . rhtml-mode) auto-mode-alist))

;; Hack: Add color to angle brackets and strings in html
(defvar html-mode-keywords
  '(("\\(<[^>]+>\\)" 1 font-lock-variable-name-face prepend)
    ("\\(\"[^\"]*\"\\)" 1 font-lock-string-face prepend)
    ("\\('[^']*'\\)" 1 font-lock-string-face prepend)))

(font-lock-add-keywords 'rhtml-mode html-mode-keywords)
(font-lock-add-keywords 'html-mode html-mode-keywords)
(font-lock-add-keywords 'html-helper-mode html-mode-keywords)

;; Hack: Add color to strings in ERB
(add-to-list 'rhtml-in-erb-keywords '("\\(#{[^>]*}\\)" .
                                      (1 font-lock-doc-face prepend)) )
(add-to-list 'rhtml-in-erb-keywords '("\\(<!--[^>]*-->\\)" .
                                      (1 font-lock-comment-face prepend)) )
(add-to-list 'rhtml-in-erb-keywords '("\\(\"[^>]*\"\\)" .
                                      (1 font-lock-string-face prepend)) )
(add-to-list 'rhtml-in-erb-keywords '("\\(\'[^>]*\'\\)" .
                                      (1 font-lock-string-face prepend)) )

