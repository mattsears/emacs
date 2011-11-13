;;----------------------------------------------------------------------------
;; Custom settings for all modes
;;----------------------------------------------------------------------------

;; Set text mode to be the default major mode
(setq default-major-mode 'text-mode)

(defvar project-root)
(setq project-root (concat (expand-file-name "~") "/emacs"))

;; Show column and line numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(column-number-mode 1)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; Make the region act quite like the text "highlight" in many apps.
(setq transient-mark-mode t)

;; Command line
(require 'cl)

;; Shutoff messages
(setq message-log-max nil)

;; Stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)

;; Set column with
(setq fill-column 80)
(setq-default fill-column 80)

;; Default tabs/indents are 2 spaces
(setq-default tab-width 2)
(setq tab-width 2)
(setq standard-indent 2)
(setq ns-pop-up-frames nil)

;; fix broken backspace
(global-set-key "\b" 'backward-delete-char)
(setq vc-handled-backends nil)

;; UTF-8 goodness
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Behaviors
(setq require-final-newline nil)
(setq display-buffer-reuse-frames t)
(setq truncate-partial-width-windows t)

;; Hitting delete will delete region and selecting a region and then
;; press a character will replace region with that character.
(pending-delete-mode 1)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; Abbrevs
(quietly-read-abbrev-file)

;; Eeek! A mouse!
(setq mac-emulate-three-button-mouse nil)

;; Automatically refresh buffers if file changes
(global-auto-revert-mode t)

;; Set window title
;;(setq frame-title-format (list '(project-root) '(": ") '(dired-directory dired-directory "%b")))
(setq user-full-name "Matt Sears")
(setq user-mail-address "matt@mattsears.com")

;; Do not add new lines with arrow down at end of buffer
(setq next-line-add-newlines nil)

;; Just say no to splash screens
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; No backups
(setq make-backup-files  nil)

;; No .saves files
(setq auto-save-list-file-name nil)

;; No auto-saving
(setq auto-save-default nil)

;; Make yes/no options y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off tool bar, scroll bar, and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode nil)
(toggle-scroll-bar -1)

;; Will somebody answer the phone!
(setq ring-bell-function (lambda () (message nil)))

;; Need an imenu
(require 'imenu)

;; Make sure we have font-lock to start with
(require 'font-lock)

;; keep searching throughout the file
(require 'find-recursive)

;; Yet another paste tool, this one for Gist (awesome)
(require 'gist)

;; Keep a history of recent changes
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

;; Sudo saving
(require 'sudo)

;; Enhanced M-x
(add-to-list 'load-path "~/.emacs.d/vendor/smex")
(setq smex-save-file "~/.emacs.bak/smex.save")
(require 'smex)
(smex-initialize)

;; Allows syntax highlighting to work, among other things
(setq global-font-lock-mode 1)
(set-face-bold-p 'bold nil)

;; Fix mouse wheel scrolling
(setq mac-emulate-three-button-mouse nil)
(global-set-key [wheel-up]'(lambda ()(interactive)(scroll-down 2)))
(global-set-key [wheel-down]'(lambda ()(interactive)(scroll-up 2)))
(setq mouse-wheel-scroll-amount '(2.1))

;; Switch windows with M-up, M-down, M-right, M-left
(windmove-default-keybindings 'meta)

;; Tramp
(setq tramp-default-method "ssh")
(custom-set-variables
 '(load-home-init-file t t))
(custom-set-faces)
(setq tramp-auto-save-directory "~/.emacs.d/.tramp-autosave/")

;; Backup settings
(setq auto-fill-mode 1)
(setq auto-save-mode nil)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq make-backup-files nil)
(setq backup-inhibited t)
(global-auto-revert-mode -1)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Misc settings
(blink-cursor-mode 1)
(setq delete-old-versions t)
(setq global-hl-line-mode 1)
(setq indicate-buffer-boundaries nil)
(setq kill-whole-line t)
(setq max-lisp-eval-depth 10000)

(setq cua-highlight-region-shift-only t)
(cua-mode t)

;; Balancing the parentheses
(setq show-paren-delay 0)
(setq show-paren-mode 1)
(setq show-paren-style 'expression)

(require 'paren)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#def")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;; No tooltips
(setq tooltip-mode nil)
(setq version-control t)
(setq mark-even-if-inactive t) ;; don't kill the mark
(setq blink-matching-paren-on-screen t)

;; No funky input for normal editing;
(set-input-method nil)

;; Ignore case when completing...filenames too
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;; Scroll with the compilation output
(setq compilation-scroll-output t)
(setq compilation-window-height 18)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Bookmarks
(setq
 bookmark-default-file "~/.emacs.d/.bookmarks" ;; keep my ~/ clean
 bookmark-save-flag 1)                        ;; autosave each change)

;; Smart pairing
;;(require 'autopair)
;;(autopair-global-mode) ;; to enable in all buffers

(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'peepopen)

(add-to-list 'load-path "~/.emacs.d/vendor/nav")
(require 'nav)

;; TAGS
(setq tags-file-name ".TAGS")

;; Speedbar options
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq speedbar-directory-button-trim-method 'trim)
(setq speedbar-track-mouse-flag nil)
(setq speedbar-smart-directory-expand-flag nil)
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-indentation-width 2)

(add-to-list 'load-path "~/.emacs.d/vendor/sr-speedbar")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-width-x 34)

(add-hook 'speedbar-reconfigure-keymaps-hook
   '(lambda ()
      (define-key speedbar-key-map (kbd "<up>") 'speedbar-prev)
      (define-key speedbar-key-map (kbd "<down>") 'speedbar-next)
      (define-key speedbar-key-map (kbd "<right>") 'speedbar-expand-line)
      (define-key speedbar-key-map (kbd "<left>") 'speedbar-contract-line )
      (define-key speedbar-key-map (kbd "M-<up>") 'speedbar-up-directory)
      (define-key speedbar-key-map (kbd "<f5>") 'speedbar-refresh)

    )
)

; Start server.
(server-start)
