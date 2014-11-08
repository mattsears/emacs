;;----------------------------------------------------------------------------
;; Colors and UI improvements
;;----------------------------------------------------------------------------

;; Set the default font-size to 16pt
(set-face-attribute 'default nil :height 160)

;; Modeline colors
(set-face-attribute 'mode-line nil
                    :foreground "#eeeeee"
                    :background "#222222"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "#7f7f7f"
                    :background "#222222"
                    :box nil)

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

;;----------------------------------------------------------------------------
;; My very own color theme for Emacs.
;;----------------------------------------------------------------------------

(defun color-theme-neptune()
  "Emacs color theme by Matt Sears created 2008-08-18"
  (interactive)
  (color-theme-install

   '(color-theme-neptune
     (
      (background-color . "#000000")
      (background-mode . dark)
      (border-color . "#dedede")
      (cursor-color . "#ff005f")
      (foreground-color . "#ffffff")
      )

     (neptune-black ((t (:foreground "#222222"))))
     (neptune-black-1 ((t (:background "#111111"))))
     (neptune-black-2 ((t (:background "#151515"))))

     (neptune-white ((t (:foreground "#eaeaea"))))
     (neptune-blue ((t (:foreground "#005fd7"))))
     (neptune-blue-1 ((t (:foreground "#7aa6da"))))
     (neptune-light-blue ((t (:foreground "#e1f3f8"))))
     (neptune-aqua ((t (:foreground "#70c0b1"))))
     (neptune-orange ((t (:foreground "#ffb964"))))
     (neptune-brown ((t (:foreground "#9b5c2e"))))
     (neptune-purple ((t (:foreground "#c397d8"))))
     (neptune-pink ((t (:foreground "#ec527a"))))
     (neptune-turquoise ((t (:foreground "#87c4bf"))))
     (neptune-green ((t (:foreground "#89ca54"))))
     (neptune-light-green ((t (:foreground "#daefa3"))))
     (neptune-red ((t (:foreground "#CF6A4C"))))
     (neptune-red-1 ((t (:foreground "#ff5f5f"))))
     (neptune-grey ((t (:foreground "#969896"))))
     (neptune-grey-1 ((t (:foreground "#7f7f7f"))))
     (neptune-gold ((t (:foreground "#cdc098"))))
     (neptune-yellow ((t (:foreground "#fad07a"))))
     (neptune-peach ((t (:foreground "#e28964"))))

     (neptune-bg ((t (:background "#000000"))))
     (neptune-highlight ((t (:background "#646c9c" :foreground "#eeeeee"))))
     (neptune-current-line ((t (:background "#080808"))))

     ;; Global colors
     (font-lock-builtin-face ((t (:inherit neptune-blue))))
     (font-lock-keyword-face ((t (:inherit neptune-red-1))))
     (font-lock-reference-face ((t (:inherit neptune-red))))
     (font-lock-comment-face ((t (:italic t :inherit neptune-grey))))
     (font-lock-comment-delimiter-face ((t (:inherit neptune-grey))))
     (font-lock-constant-face ((t (:inherit neptune-purple))))
     (font-lock-doc-string-face ((t (:inherit neptune-green))))
     (font-lock-doc-face ((t (:inherit neptune-green))))
     (font-lock-preprocessor-face ((t (:inherit neptune-green))))
     (font-lock-function-name-face ((t (:inherit neptune-yellow))))
     (font-lock-type-face ((t (:inherit neptune-purple))))
     (font-lock-regexp-grouping-backslash ((t (:inherit neptune-blue))))
     (font-lock-regexp-grouping-construct ((t (:inherit neptune-red))))
     (font-lock-string-face ((t (:inherit neptune-green))))
     (font-lock-variable-name-face ((t (:inherit neptune-blue))))
     (font-lock-warning-face ((t (:inherit neptune-red))))
     (font-lock-syntactic-keywords ((t (:inherit neptune-red))))
     (font-lock-negation-char-face ((t (nil))))

     ;; Basics
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (border ((t (:inherit neptune-bg))))
     (buffers-tab ((t (:inherit neptune-blue))))
     (lazy-highlight-face ((t (:inherit neptune-current-line))))
     (hl-line ((t (:inherit neptune-current-line))))
     (highlight ((t (:inherit neptune-current-line))))
     (highline-face ((t (:inherit neptune-pink))))
     (minibuffer-prompt ((t (:inherit neptune-pink))))
     (header-line ((t (:bold t :weight bold :underline t :foreground "grey20" :box nil))))
     (region ((t (:inherit neptune-highlight))))

     ;; Paren
     (show-paren-match-face ((t (:background "#5f005f"))))
     (show-paren-mismatch-face ((t (:inherit neptune-red))))

     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (underline ((nil (:underline nil))))
     (vertical-border ((t (nil))))

     ;; Dired
     (dired-face-directory ((t (:inherit neptune-pink))))
     (dired-face-boring ((t (:inherit neptune-pink))))
     (dired-face-executable ((t (:inherit neptune-blue))))
     (dired-face-flagged ((t (:inherit neptune-orange))))
     (dired-face-marked ((t (:inherit neptune-orange))))
     (dired-face-permissions ((t (:inherit neptune-blue))))
     (dired-face-boring ((t (:inherit neptune-grey))))
     (dired-face-header ((t (:inherit neptune-blue))))
     (dired-face-setuid ((t (:inherit neptune-red))))
     (dired-face-socket ((t (:inherit neptune-red))))
     (dired-face-symlink ((t (:inherit neptune-blue))))

     (diredp-dir-heading ((t (:inherit neptune-blue))))
     (diredp-dir-priv  ((t (:inherit neptune-blue))))

     ;; Popups
     (popup-menu-selection-face ((t (:inherit neptune-highlight))))

     (popup-scroll-bar-background-face ((t (:foreground "#ffffff" :background "#222222"))))
     (popup-scroll-bar-foreground-face ((t (:foreground "#ffffff" :background "#222222"))))
     (popup-face ((t (:background "#222222" :foreground "#ffffff" ))))
     (popup-tip-face ((t (:background "#646c9c" :foreground "#eeeeee"))))

     ;; Markdown
     (markdown-header-face ((t (:inherit neptune-blue))))
     (markdown-header-face-1 ((t (:inherit neptune-purple))))
     (markdown-list-face ((t (:inherit neptune-pink))))
     (markdown-inline-code-face ((t (:inherit neptune-gray))))
     (markdown-comment-face ((t (:inherit neptune-gray))))
     (markdown-pre-face ((t (:inherit neptune-gray))))
     (markdown-blockquote-face ((t (:inherit neptune-gray))))
     (markdown-link-face ((t (:inherit neptune-green))))

     ;; The Shell
     (eshell-prompt-face ((t (:inherit neptune-green))))
     (eshell-prompt ((t (:inherit neptune-green))))
     (eshell-ls-archive-face ((t (:inherit neptune-blue))))
     (eshell-ls-archive ((t (:inherit neptune-blue))))
     (eshell-ls-backup-face ((t (:inherit neptune-light-blue))))
     (eshell-ls-backup ((t (:inherit neptune-light-blue))))
     (eshell-ls-clutter-face ((t (:inherit neptune-white))))
     (eshell-ls-clutter ((t (:inherit neptune-white))))
     (eshell-ls-directory-face ((t (:inherit neptune-blue))))
     (eshell-ls-directory ((t (:inherit neptune-blue))))
     (eshell-ls-executable-face ((t (:inherit neptune-white))))
     (eshell-ls-executable ((t (:inherit neptune-white))))
     (eshell-ls-missing-face ((t (:inherit neptune-green))))
     (eshell-ls-missing ((t (:inherit neptune-white))))
     (eshell-ls-product-face ((t (:inherit neptune-green))))
     (eshell-ls-product ((t (:inherit neptune-green))))
     (eshell-ls-readonly-face ((t (:inherit neptune-red))))
     (eshell-ls-readonly ((t (:inherit neptune-red))))
     (eshell-ls-special-face ((t (:inherit neptune-orange))))
     (eshell-ls-special ((t (:inherit neptune-orange))))
     (eshell-ls-symlink-face ((t (:inherit neptune-blue))))
     (eshell-ls-symlink ((t (:inherit neptune-blue))))
     (eshell-ls-unreadable-face ((t (:inherit neptune-red))))
     (eshell-ls-unreadable ((t (:inherit neptune-red))))

     ;; Spelling mistakes
     (flyspell-duplicate-face ((t (:underline t))))
     (flyspell-incorrect-face ((t (:inherit neptune-red))))

     ;; Flycheck warning and error colors
     (flycheck-warning-face ((t ( :underline "#ffb964" :background nil :weight bold :underline t
))))


     ;; The left and right fringes
     (fringe ((t (:inherit neptune-black-1 ))))

     ;; Isearch
     (isearch ((t (:inherit neptune-pink))))
     (isearch-lazy-highlight-face ((t (:inherit neptune-highlight))))
     (isearch-secondary ((t (:inherit neptune-highlight))))
     (isearch-fail ((t (:inherit neptune-red))))

     ;; ibuffer
     (ibuffer-deletion-face ((t (:inherit neptune-red))))
     (ibuffer-filter-group-name-face ((t (:inherit neptune-blue))))
     (ibuffer-marked-face ((t (:inherit neptune-grey))))
     (ibuffer-title-face ((t (:inherit neptune-blue))))
     (ibuffer-dired-buffer-face ((t (:inherit neptune-blue))))
     (ibuffer-help-buffer-face ((t (:inherit neptune-blue))))
     (ibuffer-hidden-buffer-face ((t (:inherit neptune-red))))
     (ibuffer-occur-match-face ((t (:inherit neptune-red))))
     (ibuffer-read-only-buffer-face ((t (:inherit neptune-red))))
     (ibuffer-special-buffer-face ((t (:inherit neptune-red))))
     (ibuffer-title-face ((t (:inherit neptune-blue))))

     ;; ido
     (ido-first-match ((t (:inherit neptune-green))))
     (ido-first-match-face ((t (:inherit neptune-orange))))
     (ido-incomplete-regexp ((t (:box (:line-width 1 :color "red4") :foreground "red2"))))
     (ido-indicator ((t (:inherit neptune-orange))))
     (ido-indicator-face ((t (:inherit neptune-red))))
     (ido-only-match ((t (:inherit neptune-green))))
     (ido-only-match-face ((t (:inherit neptune-green))))
     (ido-subdir ((t (:inherit neptune-orange))))
     (ido-subdir-face ((t (:inherit neptune-blue))))

     ;; ERC
     (erc-default-face ((t (:inherit neptune-blue))))
     (erc-fool-face ((t (:inherit neptune-red))))
     (erc-input-face ((t (:inherit neptune-white))))
     (erc-keyword-face ((t (:inherit neptune-light-blue))))
     (erc-timestamp-face ((t (:inherit neptune-light-blue))))
     (erc-notice-face ((t (:inherit neptune-green))))
     (erc-prompt-face ((t (:inherit neptune-white))))

     ;; Full Ack
     (ack-match ((t (:foreground "#ffffff" :background "#75879e"))))
     (ack-file ((t (:foreground "#89ca54" :background "#75879e"))))
     (ack-line ((t (:foreground "#ffffff" :background "#75879e"))))

     ;; Fixme
     (font-lock-fixme-face ((t (:foreground "#ffffff" :background "#75879e"))))

     ;; Company mode

     ;; (company-preview-common ((t (:foreground nil :background ""))))
     (company-scrollbar-bg ((t (:background "#151515"))))
     (company-scrollbar-fg ((t (:background "#151515"))))
     (company-tooltip ((t (:foreground "#ffffff" :background "#1c1c1c"))))
     (company-tooltip-common ((t (:foreground "#cc6666" :background "#151515"))))
     (company-tooltip-common-selection ((t (:foreground "#c397d8" :background "#1a1d2f"))))
     (company-tooltip-selection ((t (:background "#363636"))))

     (semantic-dirty-token-face ((t (:inherit neptune-bg))))
     (semantic-unmatched-syntax-face ((t (nil))))
     (zmacs-region ((t (:inherit neptune-blue))))))
)

(color-theme-neptune)
