;;----------------------------------------------------------------------------
;; Custom key bindings
;;----------------------------------------------------------------------------

;; Newline and then indent.
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Make the delete key delete
(global-set-key [delete] 'delete-char)

;; Searching
(define-key global-map (kbd "s-F") 'ag-project-at-point)

;; Prefer backward-kill-word over Backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Open line below and go to that line.
(global-set-key (kbd "M-n") 'open-line-below)

;; Open line above and go to that line.
(global-set-key (kbd "M-p") 'open-line-above)

;; Back to indentation or beginning of line.
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)

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

;; Smex is awesome
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)

(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Vim emulation
(global-set-key (kbd "C-*") 'isearch-forward-at-point)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Leave wrapping on for vertically split windows
(setq truncate-partial-width-windows nil)

;; Not interested in email
(global-unset-key (kbd "C-x m"))
(global-unset-key "\C-z")

;; Magit status.
(global-set-key (kbd "C-x g") 'magit-status)

;; Duplicated the current line
(define-key global-map (kbd "s-d") 'duplicate-thing)

;; Align
(define-key global-map (kbd "s-M-]") 'align)

(global-set-key (kbd "<s-M-up>") 'move-line-up)
(global-set-key (kbd "<s-M-down>") 'move-line-down)

;; Indenting whole lines
(define-key global-map (kbd "s-]") 'textmate-shift-right)

;; (define-key osx-key-mode-map (kbd "A-[") 'textmate-shift-left)
(define-key global-map (kbd "s-[") 'textmate-shift-left)

;; Delete entire line without copying to kill-ring
(define-key global-map (kbd "<s-backspace>") 'matts-delete-whole-line)

;; Insert blank line
(define-key global-map (kbd "<s-return>") 'textmate-next-line)

;; Insert blank above current line
(global-set-key (kbd "A-\\") 'insert-blank-line-before-current)

;; Copy the current line
(define-key global-map (kbd "C-S-l") 'copy-line)

;; Open a file with dired
(define-key global-map (kbd "s-o") 'dired)

;; Open a file with ido
(define-key global-map (kbd "s-O") 'ido-find-file)

;; Find a file from the list of most recently open files
(define-key global-map (kbd "C-x r") 'matts-ido-choose-from-recentf)

;; Indent the buffer
(define-key global-map (kbd "s-i") 'iwb)

;; Trigger the buffer list
(define-key global-map (kbd "s-b") 'ibuffer)

;; Compile buffer in it's current mode
;; (define-key osx-key-mode-map (kbd "A-r") 'mode-compile)

;; Custom set of commands in popup window
(define-key global-map (kbd "s-m") 'matts-popup-commands)

;; Popup a window for all the methods on the buffer
;; (define-key osx-key-mode-map (kbd "A-T") 'ido-goto-symbol)

;; Find file in project
;; (global-set-key (kbd "<A-t>") 'peepopen-goto-file-gui)
(define-key global-map (kbd "s-t") 'find-file-in-project)

;; Kill the current frame, but not the window
(define-key global-map (kbd "s-w") 'my-close-current-window-asktosave)

;; Kill the other buffer and window
(define-key global-map (kbd "s-k") 'matts-close-and-delete-window)

;; Shortcut for ehell
(global-set-key "\C-x\C-z" 'eshell)

;; Remap set-mark since ctrl-space is overwritten by mac-os
(define-key  global-map  "\C-xm"  'cua-set-mark)

;; IEdit
;; (define-key osx-key-mode-map (kbd "C-;") 'iedit-mode)
(define-key global-map (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(global-set-key (kbd "M-j") (lambda ()
                              (interactive)
                              (join-line -1)))

;; Multiple cursors
(global-set-key (kbd "s-'") 'er/expand-region)
(global-set-key (kbd "s-;") 'mc/mark-next-word-like-this)
(global-set-key (kbd "s-|") 'mc/edit-lines)

(provide 'bindings)
