(deftheme neptune-theme "The neptune color theme")

(let ((class '((class color) (min-colors 89)))

      (neptune-fg       "#f8f8f2")
      (neptune-bg       "#191919")


      ;; Greens
      (neptune-green-0  "#50fa7b")
      (neptune-green-0  "#87ffad")
      (neptune-green-1  "#4af2a1")
      (neptune-green-2  "#50fa7b")


      ;; Greys
      (neptune-grey-0   "#888888")
      (neptune-grey-1   "#7f7f7f")
      (neptune-grey-2   "#151515")
      (neptune-grey-3   "#1c1c1c")
      (neptune-grey-4   "#212325")
      (neptune-grey-5   "#3a3a3a")

      ; Purples
      (neptune-purple   "#474e90")
      (neptune-purple-1 "#A78BFA")
      (neptune-purple-2 "#c678dd")

      (neptune-violet   "#A78BFA")
      (neptune-fusia    "#E879F9")

      ;; Blues
      (neptune-lightblue "#BBF0EF")
      (neptune-blue     "#8Fd4FF")
      (neptune-blue-1   "#60A5FA")

      (neptune-teal     "#5EEAD4")
      (neptune-cyan     "#00ffd7")

      (neptune-indigo   "#6366F1")
      (neptune-indigo-1 "#818CF8")


      ;; Reds
      (neptune-red-0    "#cd0000")
      (neptune-red-1    "#f44747")

      ;; Pinks
      (neptune-pink     "#ff5f87")
      (neptune-pink-1   "#ff79c6")
      (neptune-pink-2   "#e06c75")

      (neptune-rose     "#FB7185")

      ;; Oranges
      (neptune-orange   "#ffd787")
      (neptune-amber    "#FDE68A")

      ;; Yellows
      (neptune-yellow   "#FEF08A")
      (neptune-yellow-1 "#f3f89d")


      )

  (custom-theme-set-faces
   'neptune-theme

   ;;----------------------------------------------------------------------------
   ;; Base level colors
   ;;----------------------------------------------------------------------------

   `(default ((,class (:foreground ,neptune-fg :background ,neptune-bg))))
   `(header-line ((,class (:foreground ,neptune-fg))))
   `(highlight ((,class (:background ,neptune-grey-5))))
   `(hl-line ((,class (:background ,neptune-grey-4))))
   `(fringe ((,class (:foreground ,neptune-fg :background ,neptune-bg))))

   `(font-lock-comment-face ((,class (:slant italic :foreground ,neptune-grey-1))))
   `(font-lock-comment-delimiter-face ((,class (:slant italic :foreground ,neptune-grey-1))))
   `(font-lock-preprocessor-face ((,class (:foreground ,neptune-fg))))

   `(font-lock-builtin-face ((,class (:foreground ,neptune-violet))))
   `(font-lock-constant-face ((,class (:foreground , neptune-purple))))
   `(font-lock-doc-face ((,class (:foreground ,neptune-yellow))))


   `(font-lock-function-name-face ((,class (:foreground ,neptune-blue-1))))
   `(font-lock-keyword-face ((,class (:foreground ,neptune-indigo-1))))

   `(font-lock-string-face ((,class (:foreground ,neptune-green-1))))
   `(font-lock-type-face ((,class (:foreground ,neptune-amber))))
   `(font-lock-variable-name-face ((,class (:foreground ,neptune-rose))))
   `(font-lock-warning-face ((,class (:foreground ,neptune-red-1))))
   `(font-lock-negation-char-face ((,class (:foreground ,neptune-red-1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,neptune-orange :bold t))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,neptune-red-1 :bold t))))

   `(minibuffer-prompt ((,class (:bold t :foreground ,neptune-green-1))))
   `(lazy-highlight ((,class (:foreground ,neptune-red-0 :background nil))))
   `(linum ((,class (:slant italic :foreground ,neptune-grey-4))))
   `(match ((,class (:background ,neptune-pink-1))))

   ;;----------------------------------------------------------------------------
   ;; Dired mode
   ;;----------------------------------------------------------------------------

   `(dired-directory ((,class (:foreground ,neptune-purple-1 :background ,neptune-bg))))
   `(diredp-date-time ((,class (:foreground ,neptune-fg))))
   `(diredp-deletion ((,class (:foreground ,neptune-red-0 :background ,neptune-bg))))
   `(diredp-dir-heading ((,class (:foreground ,neptune-purple-1 :background ,neptune-bg))))
   `(diredp-dir-name ((,class (:foreground ,neptune-green-0 :background ,neptune-bg))))
   `(diredp-dir-priv ((,class (:foreground ,neptune-green-0 :background ,neptune-bg))))
   `(diredp-exec-priv ((,class (:foreground ,neptune-fg :background ,neptune-bg))))
   `(diredp-file-name ((,class (:foreground ,neptune-fg))))
   `(diredp-file-suffix ((,class (:foreground ,neptune-fg))))
   `(diredp-link-priv ((,class (:foreground ,neptune-fg))))
   `(diredp-number ((,class (:foreground ,neptune-fg))))
   `(diredp-no-priv ((,class (:foreground ,neptune-fg :background ,neptune-bg))))
   `(diredp-rare-priv ((,class (:foreground ,neptune-red-0 :background ,neptune-bg))))
   `(diredp-read-priv ((,class (:foreground ,neptune-fg :background ,neptune-bg))))
   `(diredp-symlink ((,class (:foreground ,neptune-red-0))))
   `(diredp-write-priv ((,class (:foreground ,neptune-fg :background ,neptune-bg))))

   `(undo-tree-visualizer-current-face ((,class :foreground ,neptune-pink-1)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,neptune-fg)))
   `(undo-tree-visualizer-register-face ((,class :foreground , neptune-orange)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,neptune-green-0)))

   `(alchemist-test--failed-face ((,class (:foreground ,neptune-red-0))))
   `(alchemist-test--success-face ((,class (:foreground ,neptune-green-0))))
   `(avy-lead-face ((,class (:foreground ,neptune-fg :background ,neptune-red-0))))
   `(avy-lead-face-0 ((,class (:foreground ,neptune-fg :background ,neptune-green-0))))
   `(bm-face ((,class (:background ,neptune-grey-4))))

   `(company-preview-common ((,class (:foreground nil :background ,neptune-bg))))
   `(company-scrollbar-bg ((,class (:background ,neptune-bg))))
   `(company-scrollbar-fg ((,class (:background ,neptune-grey-0))))
   `(company-tooltip ((,class (:foreground ,neptune-fg :background ,neptune-bg))))
   `(company-tooltip-selection ((,class (:foreground ,neptune-fg :background ,neptune-grey-4))))
   `(company-tooltip-common ((,class (:foreground ,neptune-green-0 :background ,neptune-bg))))

   `(compilation-error ((,class (:foreground ,neptune-red-0))))
   `(compilation-info ((,class (:foreground ,neptune-yellow))))
   `(compilation-line-number ((,class (:foreground ,neptune-grey-0))))
   `(compilation-mode-line-exit ((,class (:foreground ,neptune-green-0))))
   `(compilation-mode-line-fail ((,class (:foreground ,neptune-red-0))))
   `(compilation-mode-line-run ((,class (:foreground ,neptune-yellow))))

   `(ediff-odd-diff-A ((,class (:foreground ,neptune-red-0 :background ,neptune-grey-2))))
   `(ediff-odd-diff-B ((,class (:foreground ,neptune-green-0 :background ,neptune-grey-2))))
   `(emmet-preview-output ((,class (:background ,neptune-purple-1))))
   `(elixir-atom-face ((,class (:foreground ,neptune-blue))))
   `(elixir-attribute-face ((,class (:foreground ,neptune-red-0))))
   `(erc-notice-face ((,class (:foreground ,neptune-yellow))))
   `(erc-prompt-face ((,class (:foreground ,neptune-fg))))
   `(eshell-prompt ((,class (:foreground ,neptune-red-0))))
   `(eshell-ls-directory ((,class (:weight normal :foreground ,neptune-green-0))))
   `(eshell-ls-executable ((,class (:weight normal :foreground ,neptune-red-0))))
   `(eshell-ls-product ((,class (:foreground ,neptune-fg))))
   `(eshell-ls-symlink ((,class (:weight normal :foreground ,neptune-purple))))
   `(git-commit-comment-file ((,class (:foreground ,neptune-fg))))
   `(git-commit-comment-heading ((,class (:foreground ,neptune-yellow))))
   `(git-commit-summary ((,class (:foreground ,neptune-fg))))
   `(iedit-occurrence ((,class (:foreground ,neptune-red-0))))
   `(isearch ((,class (:foreground ,neptune-fg :background ,neptune-red-0))))
   `(isearch-fail ((,class (:background ,neptune-red-0))))
   `(ido-first-match ((,class (:foreground ,neptune-purple-1))))
   `(ido-only-match ((,class (:foreground ,neptune-green-0))))
   `(ido-subdir ((,class (:foreground ,neptune-fg))))
   `(ido-virtual ((,class (:foreground ,neptune-purple-1))))

   `(ivy-current-match ((,class (:background , neptune-grey-5))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,neptune-blue))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,neptune-blue))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,neptune-orange))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,neptune-orange))))

   `(diff-added          ((t (:foreground ,neptune-green-0))))
   `(diff-changed        ((t (:foreground ,neptune-yellow))))
   `(diff-removed        ((t (:foreground ,neptune-red-0))))
   `(diff-refine-added   ((t (:foreground ,neptune-green-0))))
   `(diff-refine-change  ((t (:foreground ,neptune-yellow))))
   `(diff-refine-removed ((t (:foreground ,neptune-red-0))))

   `(magit-blame-heading ((,class (:foreground ,neptune-grey-1 :background ,neptune-grey-2))))
   `(magit-branch-local ((,class (:foreground ,neptune-green-0))))
   `(magit-branch-remote ((,class (:foreground ,neptune-green-0))))
   `(magit-section-heading ((,class (:foreground ,neptune-blue :background ,neptune-grey-2))))
   `(magit-diff-added ((,class (:foreground ,neptune-green-0))))
   `(magit-diff-added-highlight ((,class (:foreground ,neptune-green-0 :inherit (magit-section-highlight)))))
   `(magit-diff-context ((,class (:foreground ,neptune-fg))))
   `(magit-diff-context-highlight ((,class (:foreground ,neptune-fg :inherit (magit-section-highlight)))))
   `(magit-diff-file-heading ((,class (:weight normal :foreground ,neptune-fg :background ,neptune-bg))))
   `(magit-diff-file-heading-highlight ((,class (:weight normal :foreground ,neptune-fg :background ,neptune-grey-2))))
   `(magit-diff-hunk-heading ((,class (:foreground ,neptune-yellow :background ,neptune-grey-2))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,neptune-yellow :background ,neptune-grey-3))))
   `(magit-diff-lines-heading ((,class (:foreground ,neptune-fg :background ,neptune-purple-1))))
   `(magit-diff-removed ((,class (:foreground ,neptune-red-0))))
   `(magit-diff-removed-highlight ((,class (:foreground ,neptune-red-0 :inherit (magit-section-highlight)))))
   `(magit-diffstat-added ((,class (:foreground ,neptune-green-0))))
   `(magit-diffstat-removed ((,class (:foreground ,neptune-red-0))))
   `(magit-hash ((,class (:foreground ,neptune-red-0))))
   `(magit-section-highlight ((,class (:background ,neptune-grey-2))))

   `(mmm-default-submode-face ((,class (:background ,neptune-bg))))

   `(mode-line ((,class (:foreground ,neptune-fg :background ,neptune-grey-3))))
   `(mode-line-inactive ((,class (:foreground ,neptune-grey-4 :background ,neptune-grey-2))))

   ;;----------------------------------------------------------------------------
   ;; Org mode
   ;;----------------------------------------------------------------------------

   `(org-checkbox ((,class (:foreground ,neptune-green-0))))
   `(org-date ((,class (:foreground ,neptune-grey-0))))
   `(org-done ((,class (:foreground ,neptune-green-0))))
   `(org-level-1 ((,class (:foreground ,neptune-blue))))
   `(org-level-2 ((,class (:foreground ,neptune-purple-1))))
   `(org-level-3 ((,class (:foreground ,neptune-green-0))))
   `(org-level-4 ((,class (:foreground ,neptune-orange))))
   `(org-level-5 ((,class (:foreground ,neptune-purple-1))))
   `(org-level-6 ((,class (:foreground ,neptune-red-0))))
   `(org-level-7 ((,class (:foreground ,neptune-blue))))
   `(org-level-8 ((,class (:foreground ,neptune-green-0))))
   `(org-link ((,class (:foreground ,neptune-blue))))
   `(org-special-keyword ((,class (:foreground ,neptune-purple-1))))
   `(org-todo ((,class (:foreground ,neptune-red-0))))

   `(region ((,class (:background ,neptune-purple))))
   `(shm-current-face ((,class (:background ,neptune-grey-4))))
   `(shm-quarantine-face ((,class (:background ,neptune-red-0))))
   `(smerge-markers ((,class (:foreground ,neptune-yellow :background ,neptune-grey-2))))
   `(smerge-mine ((,class (:foreground ,neptune-fg :background ,neptune-purple))))
   `(smerge-other ((,class (:foreground ,neptune-fg :background ,neptune-green-0))))
   `(smerge-refined-change ((,class (:foreground ,neptune-green-0))))
   `(sp-pair-overlay-face ((,class (:background ,neptune-grey-4))))
   `(sp-show-pair-match-face ((,class (:background ,neptune-grey-0))))
   `(swiper-match-face-1 ((,class (:foreground ,neptune-red-0))))
   `(swiper-match-face-2 ((,class (:foreground ,neptune-red-0))))
   `(swiper-match-face-3 ((,class (:foreground ,neptune-red-0))))
   `(swiper-match-face-4 ((,class (:foreground ,neptune-red-0))))
   `(trailing-whitespace ((,class (:background ,neptune-red-0))))

   ;;----------------------------------------------------------------------------
   ;; Neo mode (Sidebar folders)
   ;;----------------------------------------------------------------------------

   `(neo-dir-link-face ((,class (:foreground ,neptune-indigo-1))))
   `(neo-file-link-face ((,class (:foreground ,neptune-purple))))
   `(neo-dir-icon-face ((,class (:foreground ,neptune-red-0))))
   `(neo-root-dir-face ((,class (:foreground ,neptune-purple-1))))
   `(neo-button-face ((,class (:foreground ,neptune-red-0))))
   `(neo-expand-btn-face ((,class (:foreground ,neptune-purple))))


   ;;----------------------------------------------------------------------------
   ;; Web mode
   ;;----------------------------------------------------------------------------

   `(web-mode-builtin-face ((,class (:foreground ,neptune-rose))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,neptune-purple-1))))
   `(web-mode-html-tag-face ((,class (:foreground ,neptune-purple))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,neptune-purple))))
   `(web-mode-symbol-face ((,class (:foreground ,neptune-amber))))
   `(web-mode-string-face ((,class (:foreground ,neptune-green-0))))

   `(which-key-group-description-face ((,class (:foreground ,neptune-grey-0))))
   `(which-key-key-face ((,class (:foreground ,neptune-green-0))))
   `(whitespace-trailing ((,class (:background ,neptune-red-0))))

   ;;----------------------------------------------------------------------------
   ;; Markdown mode
   ;;----------------------------------------------------------------------------

   `(markdown-header-face-2 ((,class (:foreground ,neptune-blue))))
   `(markdown-header-face-3 ((,class (:foreground ,neptune-yellow))))
   `(markdown-markup-face ((,class (:foreground ,neptune-purple))))

   ;;----------------------------------------------------------------------------
   ;; Flycheck
   ;;----------------------------------------------------------------------------

   `(flycheck-info((,class (:underline (:style wave :color "Red")))))
   `(flycheck-warning((,class (:underline (:style wave :color "Red")))))
   `(flycheck-error((,class (:underline (:style wave :color "Red")))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'neptune-theme)

(provide 'config-theme)
