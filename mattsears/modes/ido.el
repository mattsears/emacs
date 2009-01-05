;; IDO bitches
(require 'ido)
(setq ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-decorations (quote ("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]")))
(setq ido-default-buffer-method (quote selected-window))
(setq ido-default-file-method (quote selected-window))
(setq ido-enable-flex-matching t)
(setq ido-enable-tramp-completion t)
(setq ido-everywhere t)
(setq ido-ignore-buffers (quote ("\\` " "\\*ECB .*\\*" "*Messages*" "*scratch*")))
(setq ido-show-dot-for-dired t)
(setq ido-use-filename-at-point (quote guess))
 
;; This tab override shouldn't be necessary given ido's default 
;; configuration, but minibuffer-complete otherwise dominates the 
;; tab binding because of my custom tab-completion-everywhere 
;; configuration.
(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-complete)))
