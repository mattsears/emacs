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
            (define-key rhtml-mode-map "\C-RET" 'zencoding-expand-line)
            ))
(setq auto-mode-alist (cons '("\\.html\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.liquid$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mustache$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . rhtml-mode) auto-mode-alist))


