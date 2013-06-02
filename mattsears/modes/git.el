;; Git goodness
;; (add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)
(autoload 'magit-status "magit" nil t)

(add-hook 'magit-mode-hook
          (lambda ()
            (setq yas/dont-activate t)
            ;; (yas/minor-mode -1)
            (set-face-background 'magit-item-highlight "#0C1021")
            (set-face-foreground 'magit-diff-add "#79b958")
            (set-face-foreground 'magit-diff-del "#d95c47")))

;; Magit status.
(global-set-key (kbd "C-x g") 'magit-status)

;; Git gutter all the things
(global-git-gutter-mode t)

(setq git-gutter:lighter " gg")
(setq git-gutter:window-width 1)
(setq git-gutter:modified-sign ".")
(setq git-gutter:added-sign "+")
(setq git-gutter:deleted-sign "-")

(set-face-foreground 'git-gutter:added "#daefa3")
(set-face-foreground 'git-gutter:deleted "#FA8072")
(set-face-foreground 'git-gutter:modified "#b18cce")
