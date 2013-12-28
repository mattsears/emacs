;;----------------------------------------------------------------------------
;; Colors and UI improvements
;;----------------------------------------------------------------------------

;; Pretty colors
(use-package color-theme
  :init
  (progn
    (color-theme-initialize)
    (setq color-theme-is-global t)
    (load-file "~/.emacs.d/color-theme-neptune.el")
    (color-theme-neptune)))

;; Set the default font-size to 16pt
(set-face-attribute 'default nil :height 180)

;; No background transparency
(setq transparency-level 100)

;; Modeline colors
(set-face-attribute 'mode-line nil
                    :foreground "#eeeeee"
                    :background "#222222"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#7f7f7f"
                    :background "#222222"
                    :box nil)

;; Make the frame transparent
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;; Turn off 3d modeline
(set-face-attribute 'mode-line nil :box nil)

(setq default-frame-alist
      (quote ((tool-bar-lines . 0)
              (fringe)
              (vertical-scroll-bars)
              (menu-bar-lines . 0)
              ;; (left-fringe . 0)
              (right-fringe . 0)
              )))

;; Skinny cursor
(setq-default cursor-type '(bar . 2))

;; No blinking curstor
(blink-cursor-mode -1)

;; Show column and line numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(column-number-mode 1)

;; Stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)
(setq x-select-enable-clipboard nil)
(setq select-active-regions nil)

;; Set column with
(setq fill-column 80)
(setq-default fill-column 80)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; Eeek! A mouse!
(setq mac-emulate-three-button-mouse nil)

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

;; Allows syntax highlighting to work, among other things
(setq global-font-lock-mode 1)
(set-face-bold-p 'bold nil)

;; Make the cursor blnk
(blink-cursor-mode 1)

;; No tooltips
(setq tooltip-mode nil)

;;----------------------------------------------------------------------------
;; Diff mode cosmetics
;;----------------------------------------------------------------------------

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "#9fab7d")
     (set-face-foreground 'diff-removed "#CF6A4C")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "#9fab7d")
     (set-face-foreground 'magit-diff-del "#CF6A4C")))
