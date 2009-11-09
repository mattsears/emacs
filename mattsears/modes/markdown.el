;;----------------------------------------------------------------------------
;; Markdown mode options
;;----------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.article" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.comment" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (define-key markdown-mode-map (kbd "<tab>") 'defunkt-indent)))

