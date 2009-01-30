;;; Set org mode to be the default major mode
(setq default-major-mode 'org-mode)

(defvar project-root)
(setq project-root (concat (expand-file-name "~") "/"))

;; always need a good font
(set-default-font "-apple-monaco-medium-r-normal--0-0-0-0-m-0-mac-roman")
(set-face-font 'default "-apple-monaco-medium-r-normal--0-0-0-0-m-0-mac-roman")
(set-face-font 'modeline "-apple-monaco-medium-r-normal--0-0-0-0-m-0-mac-roman")
(set-face-font 'minibuffer-prompt "-apple-monaco-medium-r-normal--0-0-0-0-m-0-mac-roman")

;; Modify the mode-line as well. This is a cleaner setup than the
(setq default-mode-line-format
      '(" "
       mode-line-frame-identification
       mode-line-buffer-identification
       "  "
       global-mode-string
       "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]  "
       (line-number-mode "Line %l  ")
       (column-number-mode "Column %c  ")
       (-3 . "%p")
       "% "))

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;;  makes the region act quite like the text "highlight" in many apps.
(setq transient-mark-mode nil)

;; Shutoff messages
(setq message-log-max nil)

;; stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)

;; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-primary nil)

;; Copy-paste should work with other X clients
(setq x-select-enable-clipboard t)
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; highlighting doesn't overwrite the clipboard or alter the kill ring,
;; but you can paste in merely highlightedtext with the mouse if you want to)
(setq select-active-regions t) ;  active region sets primary X11 selection
(setq yank-pop-change-selection t)

(setq kill-whole-line nil)

;; Set column with to 100
(setq fill-column 100)

;; Default tabs/indents are 4 spaces
(setq-default tab-width 4)
(setq tab-width 4)
(setq standard-indent 4)

;; Behaviors
(prefer-coding-system 'utf-8)
(setq line-number-mode t)
(setq column-number-mode t)
(setq require-final-newline t)
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

;; Set window title
(setq frame-title-format (list '("mattsears: ") '(dired-directory dired-directory "%b")))
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
(setq make-backup-files         nil)

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

;; Turn off that damn bell and flashing!!
(setq ring-bell-function (lambda () (message nil)))

;; Need on imenu
(require 'imenu)

;; Need imdb because I am a fan of Kevin Bacon
(require 'imdb)

;; Make sure we have font-lock to start with
(require 'font-lock)

;; keep searching throughout the file
(require 'find-recursive)

;; Find files in project
(require 'find-file-in-project)
;(setq find-file-in-project-file-excludes (list "vendor" "fixtures"))

;; Allows syntax highlighting to work, among other things
(setq global-font-lock-mode 1)

;; Fix mouse wheel scrolling
(setq mac-emulate-three-button-mouse nil)
(global-set-key [wheel-up]'(lambda ()(interactive)(scroll-down 2)))
(global-set-key [wheel-down]'(lambda ()(interactive)(scroll-up 2)))
(setq mouse-wheel-scroll-amount '(2.1))

;; Remember where I left off
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

; Set encoding
(prefer-coding-system 'utf-8)

;; Start the server so we can use emacsclient to open files
(server-start)

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

; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Misc settings
(blink-cursor-mode 1)
(setq delete-old-versions t)
(setq global-font-lock-mode 1)
(setq global-hl-line-mode 1)
(setq indicate-buffer-boundaries nil)
(setq kill-whole-line t)
(setq max-lisp-eval-depth 10000)

;; Balancing the parentheses
(setq show-paren-delay 0)
(setq show-paren-mode t)
(setq show-paren-style 'expression)

; No tooltips
(setq tooltip-mode nil)
(setq version-control t)
(setq mark-even-if-inactive t) ;; don't kill the mark
(setq blink-matching-paren-on-screen t)

;; For it's awesome rectangle mode.
(setq cua-enable-cua-keys nil)
(setq cua-toggle-set-mark nil)
(cua-mode)

; No funky input for normal editing;
(set-input-method nil)

; Ignore case when completing...filenames too
(setq completion-ignore-case t
  read-file-name-completion-ignore-case t)

;;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook
         'ansi-color-for-comint-mode-on)

;; Resize the frame
(require 'maxframe)
(setq mf-max-width 1800)  ;; Pixel width of main monitor.
(setq mf-max-height 1100)
(add-hook 'window-setup-hook
       (lambda ()
         (reset-window-position)
	))

;; Fix foolish calendar-mode scrolling.
(add-hook 'calendar-load-hook
 '(lambda ()
 (setq mark-holidays-in-calendar t)
 (define-key calendar-mode-map ">" 'scroll-calendar-left)
 (define-key calendar-mode-map "<" 'scroll-calendar-right)
 (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
 (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))

(require 'browse-kill-ring)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
   "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
 "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)


;; Start server.
(server-start)
