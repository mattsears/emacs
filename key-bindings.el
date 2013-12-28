;;----------------------------------------------------------------------------
;; Custom key bindings
;;----------------------------------------------------------------------------

;; Newline and then indent.
(global-set-key (kbd "RET") 'smart-newline)

;; Make the delete key delete
(global-set-key [delete] 'delete-char)

;; Searching
(define-key global-map (kbd "s-F") 'ack)
(define-key global-map (kbd "C-c F") 'ack)

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

;; Delete entire line without copying to kill-ring
(define-key global-map (kbd "<s-backspace>") 'matts-delete-whole-line)

;; Insert blank line
(define-key global-map (kbd "<s-return>") 'open-line-below)

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

;; Custom set of commands in popup window
(define-key global-map (kbd "s-m") 'matts-quick-menu)

;; Popup a window for all the methods on the buffer
;;(define-key osx-key-mode-map (kbd "A-T") 'ido-goto-symbol)

;; Find file in project
(global-set-key (kbd "s-t") 'fiplr-find-file)

;; Find tag in project
(define-key global-map (kbd "s-.") 'my-ido-find-tag)

;; Kill the current frame, but not the window
(define-key global-map (kbd "s-w") 'my-close-current-window-asktosave)
(define-key global-map (kbd "C-c w") 'my-close-current-window-asktosave)

;; Kill the other buffer and window
(define-key global-map (kbd "s-k") 'matts-close-and-delete-window)

;; Shortcut for ehell
(global-set-key "\C-x\C-z" 'eshell)

;; Remap set-mark since ctrl-space is overwritten by mac-os
(define-key  global-map  "\C-xm"  'cua-set-mark)

;; Multiple Cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "M-j") (lambda ()
                              (interactive)
                              (join-line -1)))

;; Multiple cursors
(global-set-key (kbd "s-'") 'er/expand-region)
(global-set-key (kbd "s-;") 'mc/mark-next-word-like-this)
(global-set-key (kbd "s-|") 'mc/edit-lines)

;; Expand Regions
(global-set-key (kbd "C-'") 'er/mark-inside-quotes)

(global-set-key (kbd "C-c d") 'my-rails-database)
(global-set-key (kbd "C-Z") 'redo)
; (global-set-key (kbd "C-c e") 'project-explorer-open)

;; Escape closes minibuffer
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)

;; Find file in project
(define-key global-map (kbd "s-p") 'matts-ido-find-project)
(define-key global-map (kbd "C-c p") 'matts-ido-find-project)

;; Use projectile's find file
(define-key global-map [(super t)] 'projectile-find-file)

;; Comment or comment lines
(define-key global-map (kbd "s-/") 'comment-or-uncomment-line-or-region)

;; Shifts text left or right
(define-key global-map (kbd "s-]") 'text-shift-right)
(define-key global-map (kbd "s-[") 'text-shift-left)

(provide 'bindings)
