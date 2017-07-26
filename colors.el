;;----------------------------------------------------------------------------
;; Colors and UI improvements
;;----------------------------------------------------------------------------

;; Set the default font-size to 16pt
(set-face-attribute 'default nil :height 180)

;;----------------------------------------------------------------------------
;; Diff mode cosmetics
;;----------------------------------------------------------------------------

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "#9fab7d")
     (set-face-foreground 'diff-removed "#CF6A4C")))

(defun color-theme-jellybeans ()
  (interactive)

  (color-theme-install
   '(color-theme-jellybeans
     (
      (background-color . "#1d1f21")
      (background-mode . dark)
      (border-color . "#1c1c1c")
      (cursor-color . "#b0d0f0")
      (foreground-color . "#e8e8d3")
      (mouse-color . "#151515"))

     (fringe ((t (:background "#1c1c1c"))))
     (mode-line ((t (:foreground "#eeeeee" :background "#222222" :box nil))))
     (mode-line ((t (:foreground "#7f7f7f" :background "#222222" :box nil))))
     (region ((t (:background "#646c9c"))))
     (font-lock-builtin-face ((t (:foreground "#8fbfdc"))))
     (font-lock-constant-face ((t (:foreground "#c3c2ef"))))
     (font-lock-comment-face ((t (:foreground "#888888"))))
     (font-lock-function-name-face ((t (:foreground "#fad07a"))))
     (font-lock-keyword-face ((t (:foreground "#7facca"))))
     (font-lock-string-face ((t (:foreground "#99ad6a"))))
     (font-lock-type-face ((t (:foreground"#ffd080"))))

     (font-lock-variable-name-face ((t (:foreground "#e36049"))))
     (font-lock-preprocessor-face ((t (:foreground "#8fbfdc"))))
     (trailing-whitespace ((t (:background "#252525"))))
     (minibuffer-prompt ((t (:foreground "#8fbfdc" :bold t))))
     (highlight ((t (:background "#1c1c1c"))))
     (hl-line ((t (:background "#1c1c1c"))))
     (show-paren-match ((t (:foreground "#ffffff" :background "#7a81ff"))))
     (font-lock-warning-face ((t (:foreground "#e36049" :bold t))))

     (web-mode-keyword-face ((t (:foreground "#7facca"))))
     (web-mode-html-tag-face ((t (:foreground "#7facca"))))
     (web-mode-html-attr-name-face ( (t (:foreground"#ffd080")) ))
     (web-mode-html-attr-value-face ( (t (:foreground "#99ad6a")) ))

     (diff-added          ((t (:background "#335533" :foreground ,zenburn-green))))
     (diff-changed        ((t (:background "#555511" :foreground ,zenburn-yellow-1))))
     (diff-removed        ((t (:background "#553333" :foreground ,zenburn-red-2))))
     (diff-refine-added   ((t (:background "#338833" :foreground ,zenburn-green+4))))
     (diff-refine-change  ((t (:background "#888811" :foreground ,zenburn-yellow))))
     (diff-refine-removed ((t (:background "#883333" :foreground ,zenburn-red))))

     )))

(color-theme-jellybeans)
