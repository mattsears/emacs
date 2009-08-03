;; bindings.el - Custom key bindings

;; Newline and then indent.
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Re-assign the tab key
(global-set-key (kbd "TAB") 'insert-soft-tab);

;; make the delete key delete
(global-set-key [delete] 'delete-char)

;; Open line below and go to that line.
(global-set-key (kbd "M-n") 'open-line-below)

;; Open line above and go to that line.
(global-set-key (kbd "M-p") 'open-line-above)

;; Back to indentation or beginning of line.
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [f4] 'recentf-open-files)

;; Split windows
(global-set-key [f6] 'split-window-vertically)
(global-set-key [f7] 'split-window-horizontally)
(global-set-key [f8] 'delete-window)

;; Buffers
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "<C-tab>") 'ibuffer)
(global-set-key "\C-x\C-b" 'buffer-menu)

;; General
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\M-i" 'insert-soft-tab)
(global-set-key "\C-R" 'replace-string)

;; Use ido to match commands in the mini-buffer
;;(global-set-key "\M-x" 'ido-execute-command)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;; Magit status.
(global-set-key (kbd "C-x g") 'magit-status)

;; Duplicated the current line
(define-key osx-key-mode-map (kbd "A-d") 'duplicate-line)

;; Open a file with dired
(define-key osx-key-mode-map (kbd "A-o") 'dired)

;; Open a file with ido
(define-key osx-key-mode-map (kbd "A-O") 'ido-find-file)

;; Find a file from the list of most recently open files
(define-key osx-key-mode-map (kbd "C-x r") 'matts-ido-choose-from-recentf)

;; Indent the buffer
(define-key osx-key-mode-map (kbd "A-i") 'iwb)

;; Trigger the buffer list
(define-key osx-key-mode-map (kbd "A-b") 'ibuffer)

;; Compile buffer in it's current mode
(define-key osx-key-mode-map (kbd "A-r") 'mode-compile)

;; Custom set of commands in popup window
(define-key osx-key-mode-map (kbd "A-m") 'matts-popup-commands)

;; Popup a window for all the methods on the buffer
(define-key osx-key-mode-map (kbd "A-T") 'matts-popup-symbols)

;; Find file in project
(define-key osx-key-mode-map (kbd "A-t") 'find-file-in-project)

;; Kill the current frame, but not the window
(define-key osx-key-mode-map (kbd "A-w") 'my-close-current-window-asktosave)

;; Kill the current buffer and window
(define-key osx-key-mode-map (kbd "A-W") 'matts-close-and-delete-window)

;; Delete entire line without copying to kill-ring
(define-key osx-key-mode-map (kbd "<A-backspace>") 'matts-delete-whole-line)

;; Shortcut for ehell
(global-set-key "\C-x\C-z" 'eshell)

;; Cancel current command
(global-set-key [(alt .)] 'keyboard-quit)

(provide 'bindings)
