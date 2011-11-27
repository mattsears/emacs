;;----------------------------------------------------------------------------
;; Custom key bindings
;;----------------------------------------------------------------------------

;; Newline and then indent.
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; make the delete key delete
(global-set-key [delete] 'delete-char)

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
(define-key osx-key-mode-map (kbd "<C-tab>") 'ibuffer)
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

;; Comment or uncomment the entire line
(define-key global-map (kbd "A-/") 'comment-or-uncomment-region-or-line)

;; Align
(define-key global-map (kbd "A-M-]") 'align)

(global-set-key (kbd "<A-M-up>") 'move-text-up)
(global-set-key (kbd "<A-M-down>") 'move-text-down)

;; Indenting whole lines
(define-key osx-key-mode-map (kbd "A-]")  'textmate-shift-right)
(define-key osx-key-mode-map (kbd "A-[") 'textmate-shift-left)

;; Delete entire line without copying to kill-ring
(define-key osx-key-mode-map (kbd "<A-backspace>") 'matts-delete-whole-line)

;; Insert blank line
(define-key global-map [M-return] 'insert-blank-line-after-current)

;; Insert blank above current line
(global-set-key (kbd "A-\\") 'insert-blank-line-before-current)

;; Copy the current line
(define-key global-map (kbd "C-S-l") 'copy-line)

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
;;(define-key osx-key-mode-map (kbd "A-r") 'mode-compile)

;; Custom set of commands in popup window
(define-key osx-key-mode-map (kbd "A-m") 'matts-popup-commands)


;; Popup a window for all the methods on the buffer
(define-key osx-key-mode-map (kbd "A-T") 'ido-goto-symbol)

;; Find file in project
;;(define-key osx-key-mode-map (kbd "A-t") 'find-file-in-project)
(define-key osx-key-mode-map (kbd "A-t") 'peepopen-goto-file-gui)
(global-set-key (kbd "<A-t>") 'peepopen-goto-file-gui)

;; Kill the current frame, but not the window
(define-key osx-key-mode-map (kbd "A-w") 'my-close-current-window-asktosave)

;; Kill the other buffer and window
;;(global-set-key (kbd "<A-W>") 'matts-close-and-delete-window)
(define-key osx-key-mode-map (kbd "A-k") 'matts-close-and-delete-window)

;; Shortcut for ehell
(global-set-key "\C-x\C-z" 'eshell)

;; Key-chord bindings

(add-to-list 'load-path "~/.emacs.d/vendor/key-chord.el")
(require 'key-chord)
(key-chord-mode 1)

;; Search github
(key-chord-define-global "gh" 'search-github)
(key-chord-define-global "er" 'iedit-mode)

(provide 'bindings)
