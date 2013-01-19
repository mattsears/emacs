;; Git goodness
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)
(autoload 'magit-status "magit" nil t)

(add-hook 'magit-mode-hook
          (lambda ()
            (setq yas/dont-activate t)
            (yas/minor-mode -1)
            (set-face-background 'magit-item-highlight "#0C1021")
            (set-face-foreground 'magit-diff-add "#79b958")
            (set-face-foreground 'magit-diff-del "#d95c47")))

;; Magit status.
(global-set-key (kbd "C-x g") 'magit-status)
