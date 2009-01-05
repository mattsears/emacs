;; Split windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)

;; Buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; Easy buffer switching
(require 'wcy-swbuff)
(global-set-key (kbd "<C-tab>") 'wcy-switch-buffer-forward)
(global-set-key (kbd "<C-S-kp-tab>") 'wcy-switch-buffer-backward)

;; General
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\M-i" 'insert-soft-tab)
(global-set-key "\C-R" 'replace-string)

;; Vim emulation
(global-set-key (kbd "C-*") 'isearch-forward-at-point)
 
;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Leave wrapping on for vertically split windows
(setq truncate-partial-width-windows nil)

;; Not interested in printing
(when (boundp 'osx-key-mode-map)
 (define-key osx-key-mode-map (kbd "A-p") 
   '(lambda () (interactive) (message "noop"))))
 
;; Not interested in email
(global-unset-key (kbd "C-x m"))
(global-unset-key "\C-z")

;; Duplicated the current line
(define-key osx-key-mode-map (kbd "A-d") 'duplicate-line)

;; Close the buffer not the window
(define-key osx-key-mode-map (kbd "A-w") 'kill-this-buffer)

;; Open a file with find-file
(define-key osx-key-mode-map (kbd "A-o") 'find-file)

;; Indent the buffer
(define-key osx-key-mode-map (kbd "A-b") 'iwb)

;; Shell
(global-set-key "\C-x\C-z" 'shell) ; shortcut for shell

;; Cancel current command
(global-set-key [(alt .)] 'keyboard-quit)

(provide 'bindings)
