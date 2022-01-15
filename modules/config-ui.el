;; Set the default font size (*100)
(set-face-attribute 'default nil :height 160)

(setq-default mode-line-format
              (list
               '(:eval (propertize (concat " [" (substring vc-mode 5) "]")
                                   'face 'font-lock-builtin-face))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %f "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face 'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               ;; line and column
               " (" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%C" 'face 'font-lock-constant-face) ;; % above top
               ;; (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ (string-width " %m ") (+ 3 (string-width mode-name)))
                                              )))))
               ;; the current major mode
               (propertize " %m " 'face '(:foreground "#5DD8FF"))
               ))

;; Highlight the current line
(use-package highline
  :init
  (progn
    (global-hl-line-mode)
    ))

;; Paren - Change the default colors for matching parens
(use-package paren
  :init
  (progn
    (show-paren-mode)
    (set-face-background 'show-paren-match "#1d1f21")
    (set-face-foreground 'show-paren-match "#ff5f87")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
    ))


;; Briefly highlights where your cursor is located when switching buffers
(use-package beacon
  :config
  (setq beacon-color "#00ffd7")
  (progn
    (beacon-mode 1)
    ))

;;----------------------------------------------------------------------------
;; Smooth scrolling
;;----------------------------------------------------------------------------

(use-package smooth-scrolling
  :init
  (setq redisplay-dont-pause t
        scroll-margin 1
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1))

;;----------------------------------------------------------------------------
;; Shell customizations
;;----------------------------------------------------------------------------
                                        ;
(use-package rainbow-mode
  :hook prog-mode)

(use-package rainbow-delimiters
  :defer 1
  :hook (prog-mode . rainbow-delimiters-mode))

;; Custom character highlights
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(
           ("<%=" 0 'font-lock-constant-face prepend)
           ("<%" 0 'font-lock-constant-face prepend)
           ("%>" 0 'font-lock-constant-face prepend)
           ("-%>" 0 'font-lock-constant-face prepend)
           )'end))
      '(html-mode nxml-mode web-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-project-detection 'auto)
  (set-face-attribute 'mode-line nil :family "Noto Sans" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 100)
  )

(provide 'config-ui)
