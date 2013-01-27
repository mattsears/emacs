;;----------------------------------------------------------------------------
;; Markdown mode options
;;----------------------------------------------------------------------------

(vendor 'markdown-mode)
(require 'markdown-mode)

(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mkd" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.article" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.comment" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.simplenote\.*" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (define-key markdown-mode-map (kbd "<tab>") 'defunkt-indent)))

