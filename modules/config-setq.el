;;----------------------------------------------------------------------------
;; Global settings for all modes
;;----------------------------------------------------------------------------

;; Set the default mode to tex-mode
(setq-default major-mode 'shell-script-mode)

;; Indicate where the 100 column character ends
(require 'fill-column-indicator)
(setq fci-rule-use-dashes nil)
(setq fci-rule-color "#3a3a3a")
(setq-default fci-rule-column 100)
(setq display-fill-column-indicator-column 100)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; Make the region act quite like the text "highlight" in many apps.
(setq transient-mark-mode t)

;; Default tabs/indents are 2 spaces
(setq-default tab-width 2)
(setq tab-width 2)
(setq standard-indent 2)

;; No backups
(setq make-backup-files nil)

(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup/.cache/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup/.cache/" t)))

;; No .saves files
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix
      "~/.emacs.d/.cache/auto-save-list/.saves-")

;; Scroll to where you last left off
(setq save-place-file "~/.emacs.d/.saveplace")
(save-place-mode 1)

;; Make yes/no options y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup settings
(setq auto-fill-mode 1)
(setq auto-save-mode nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Misc settings
(setq indicate-buffer-boundaries nil)

;; This will help distinguish files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; No blinky
(blink-cursor-mode 0)

;; Bookmarks are good.  I need to use them more.
(setq
 bookmark-default-file (dot-emacs ".bookmarks")
 bookmark-save-flag 1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Auto associate files that emacs doesn't know what to do with otherwise
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("hosts" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Cask" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".*mutt.*" . message-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".aws" . shell-script-mode) auto-mode-alist))

;; Set column with
(setq fill-column 80)
(setq-default fill-column 80)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; Just say no to splash screens
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Turn off tool bar, scroll bar, and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key [escape] 'minibuffer-keyboard-quit)

(setq auth-sources '("~/.authinfo"))

;;You can replace the active region just by typing text
(delete-selection-mode 1)

;; This made custom themese work
(setq custom--inhibit-theme-enable nil)

;; Don't create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Don't create lock files either
(setq create-lockfiles nil)

;; Set vterm-toggle buffer at the bottom
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
               (display-buffer-reuse-window display-buffer-at-bottom)
               (reusable-frames . visible)
               (window-height . 0.4)))

(provide 'config-setq)
