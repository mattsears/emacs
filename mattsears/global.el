;; always need a good font
(set-default-font "-apple-monaco-medium-r-normal--0-0-0-0-m-0-mac-roman") 

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
;(one-buffer-one-frame-mode 0)

;; text mode improvements
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (auto-fill-mode 1)
             ))

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

;; Now C-y will paste from the clipboard
(setq x-select-enable-clipboard t)

;; Make yes/no options y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off tool bar, scroll bar, and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode nil)
(toggle-scroll-bar -1)

;; Turn off that damn bell and flashing!!
(setq ring-bell-function (lambda () (message nil)))

;; Make sure we have font-lock to start with
(require 'font-lock)

;; keep searching throughout the file
(require 'find-recursive)

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

;; Ignore certain files when switching buffers
(require 'iswitchb)  
(add-to-list 'iswitchb-buffer-ignore "^ ")
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*ECB")
(add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "*ftp ")
(add-to-list 'iswitchb-buffer-ignore "*bsh")
(add-to-list 'iswitchb-buffer-ignore "*jde-log")
(add-to-list 'iswitchb-buffer-ignore "^[tT][aA][gG][sS]$")

;; Backup settings
(setq auto-fill-mode 1)
(setq auto-save-mode nil)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq make-backup-files nil)
;(setq backup-inhibited 1 t)

;; Misc settings
(setq blink-cursor-mode nil)
(setq delete-old-versions t)
(setq global-font-lock-mode 1)
(setq global-hl-line-mode 1)
(setq indicate-buffer-boundaries nil)
(setq kill-whole-line t)
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size -1)
(setq show-paren-mode nil)
(setq text-mode-hook nil)
(setq tooltip-mode nil)
(setq version-control t)

; No funky input for normal editing;
(set-input-method nil)             

; Copy-paste should work with other X clients
(setq x-select-enable-clipboard t)  
(setq interprogram-paste-function   
  'x-cut-buffer-or-selection-value)

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
