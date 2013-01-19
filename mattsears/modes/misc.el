;; Other text mode improvements
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (auto-fill-mode 1)
             ))
;; Bash
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))

;; Mode compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key (kbd "C-c c") 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c k") 'mode-compile-kill)

;; Indicate syntax errors
(vendor 'flycheck)
(require 'flycheck)
;; Enable flymake for all files
(add-hook 'find-file-hook 'flycheck-mode)
;; (add-hook 'ruby-mode-hook 'flycheck-mode)
;; (add-hook 'coffee-mode-hook 'flycheck-mode)
;; (add-hook 'sass-mode-hook 'flycheck-mode)
;; (add-hook 'haml-mode-hook 'flycheck-mode)

;; Midnight mode to clean up old buffers
(require 'midnight)

;; Ack
(vendor 'full-ack)
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(add-to-list 'load-path "~/.emacs.d/vendor/mustache-mode.el")
(require 'mustache-mode)

;; Simplenotes
(vendor 'simplenote)
(require 'simplenote)

;; Graphically indicates the fill column
(setq fill-column 80)
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "#1a1d2f")
(add-hook 'ruby-mode-hook 'fci-mode)
(add-hook 'haml-mode-hook 'fci-mode)
(add-hook 'markdown-mode-hook 'fci-mode)
(add-hook 'coffee-mode-hook 'fci-mode)
(add-hook 'js-mode-hook 'fci-mode)
(add-hook 'espresso-mode-hook 'fci-mode)
(add-hook 'text-mode-hook 'fci-mode)

;; Iedit mode
(vendor 'iedit)
(require 'iedit)

;; Nyan nyan nyan
(vendor 'nyan-mode)
(require 'nyan-mode)

;; Handy way to expand regions (https://github.com/magnars/expand-region.el)
(vendor 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-\"") 'er/expand-region)

(vendor 'dash)
(require 'dash)

(vendor 'smartparens)
(require 'smartparens)
(smartparens-global-mode t)
