;;----------------------------------------------------------------------------
;; Global settings for all modes
;;----------------------------------------------------------------------------
(setq ns-use-native-fullscreen nil)

;; Set text mode to be the default major mode
(setq default-major-mode 'text-mode)

;; Control the font size
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; Make the region act quite like the text "highlight" in many apps.
(setq transient-mark-mode t)

;; Shutoff messages
(setq message-log-max nil)

;; Default tabs/indents are 2 spaces
(setq-default tab-width 2)
(setq tab-width 2)
(setq standard-indent 2)
(setq ns-pop-up-frames nil)
(setq tab-always-indent 'complete)

;; Fix broken backspace
(global-set-key "\b" 'backward-delete-char)
(setq vc-handled-backends nil)

;; UTF-8 goodness
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
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

;; Abbrevs
(quietly-read-abbrev-file)

;; Set window title
(setq user-full-name "Matt Sears")
(setq user-mail-address "matt@mattsears.com")

;; Do not add new lines with arrow down at end of buffer
(setq next-line-add-newlines nil)

;; No backups
(setq make-backup-files  nil)

;; No .saves files
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix
      "~/.emacs.d/.cache/auto-save-list/.saves-")

;; No auto-saving
(setq auto-save-default nil)

;; Make yes/no options y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Will somebody answer the phone!
(setq ring-bell-function (lambda () (message nil)))

;; Need an imenu
(require 'imenu)

;; Make sure we have font-lock to start with
(require 'font-lock)

;; Allows syntax highlighting to work, among other things
(setq global-font-lock-mode 1)
(set-face-bold-p 'bold nil)

;; Fix mouse wheel scrolling
(setq mac-emulate-three-button-mouse nil)
(global-set-key [wheel-up]'(lambda ()(interactive)(scroll-down 4)))
(global-set-key [wheel-down]'(lambda ()(interactive)(scroll-up 4)))
(setq mouse-wheel-scroll-amount '(2.1))

;; Switch windows with M-up, M-down, M-right, M-left
(windmove-default-keybindings 'meta)

;; Tramp
(setq tramp-default-method "ssh")
(custom-set-variables
 '(load-home-init-file t t))
(custom-set-faces)
(setq tramp-auto-save-directory
      (dot-emacs ".tramp-autosave/"))

;; Backup settings
(setq auto-fill-mode 1)
(setq auto-save-mode nil)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Misc settings
(blink-cursor-mode 1)
(setq delete-old-versions t)
(setq global-hl-line-mode 1)
(setq indicate-buffer-boundaries nil)
(setq kill-whole-line t)
(setq max-lisp-eval-depth 10000)

(setq cua-highlight-region-shift-only t)
(cua-mode t)

;; No tooltips
(setq tooltip-mode nil)
(setq version-control t)
(setq mark-even-if-inactive t) ;; don't kill the mark
;; (setq blink-matching-paren-on-screen t)

;; No funky input for normal editing;
(set-input-method nil)

;; Ignore case when completing...filenames too
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; Scroll with the compilation output
(setq compilation-scroll-output t)
(setq compilation-window-height 18)

;; This will help distinguish files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Bookmarks are good.  I need to use them more.
(setq
 bookmark-default-file (dot-emacs ".bookmarks")
 ;; autosave each change)
 bookmark-save-flag 1)

;; TAGS
(setq tags-file-name ".TAGS")

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Save the open buffers so they reopen when restarting emacs
;; Automatically save and restore sessions
(require 'desktop)
(desktop-save-mode 1)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;; String tool all the things
;; (add-to-list 'load-path "~/.emacs.d/vendor/string-tools.el")
;; (require 'string-tools)
;; (setq global-string-tools-mode 1)

;; Set custom calendar colors
(custom-set-faces
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 1.5 :inherit variable-pitch))))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-toolbar ((t :foreground "white" :background "#1a1d2f")))
 '(cfw:face-toolbar-button-off ((t :foreground "Gray40" :background "#1a1d2f" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :background "#1a1d2f" :weight bold))))

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:ical-create-source "Moon" "http://www.google.com/calendar/ical/matt%40littlelines.com/public/basic.ics" "Gray")  ; ICS source1
    )))

;; Enable debug on error so we can see what's going on
;;(setq debug-on-error t)

;; Other text mode improvements
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (auto-fill-mode 1)
             ))

;; Bash
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))

;; Mode compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key (kbd "C-c c") 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c k") 'mode-compile-kill)

;; Midnight mode to clean up old buffers
(require 'midnight)

;; inserting text while the mark is active causes the selected text to be deleted first.
(delete-selection-mode t)

(provide 'global)
