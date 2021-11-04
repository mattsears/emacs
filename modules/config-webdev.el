
;;----------------------------------------------------------------------------
;; Web mode
;;----------------------------------------------------------------------------

(use-package web-mode
  :mode (("\\.ejs\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.liquid\\'" . web-mode)
         ("\\.eex\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.html?\\'" . web-mode)
         )
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-closing t)

    ;; (setq web-mode-attr-indent-offset 4)
    ;; (setq web-mode-attr-value-indent-offset 4)
    (setq web-mode-block-padding 6)
    )
  :init
  (progn
    (add-hook 'web-mode-hook
              (lambda ()
                (whitespace-mode -1)
                ;; (yas-minor-mode -1)
                ))
    )
  ;; Removes the pesky issue with evil esacape and inserting a snippet for some reason
  (remove-hook 'web-mode-hook 'snippet-insert)
  )

;;----------------------------------------------------------------------------
;; Css/Sass/Haml modes
;;----------------------------------------------------------------------------

(use-package css-mode
  :init
  ;; (progn
    ;; (hexcolour-add-to-font-lock))
  :config
  (progn
    (setq cssm-indent-level 2)
    (setq cssm-newline-before-closing-bracket t)
    (setq cssm-indent-function #'cssm-c-style-indenter)

    (setq tab-width 2)
    (define-key css-mode-map [return] 'newline-and-indent)
    (setq css-electric-brace-behavior t)
    (setq css-electric-semi-behavior t)
    (setq css-indent-offset 2)
    (setq css-indent-offset 2)))

(use-package sass-mode
  :config
  (progn
    (add-hook 'sass-mode-hook 'my-sass-comment-fix)
    (add-hook 'sass-mode-hook 'hexcolour-add-to-font-lock)
    ))

(use-package haml-mode
  :mode (("\\.haml$" . haml-mode))
  :config (setq haml-backspace-backdents-nesting nil))

(use-package slim-mode
  :mode (("\\.slim$" . slim-mode))
  :config ())

;;----------------------------------------------------------------------------
;; Prettyifies javascript, html, and css files.
;;----------------------------------------------------------------------------

(use-package web-beautify
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))

;;----------------------------------------------------------------------------
;; Coffeescript mode
;;----------------------------------------------------------------------------

(use-package coffee-mode
  :defer 2
  :config
  (progn
    (set (make-local-variable 'tab-width) 2)
    (setq coffee-js-mode 'javascript-mode)
    (setq coffee-args-compile '("-c" "--no-wrap"))
    (setq coffee-debug-mode t)
    (setq coffee-command "/usr/local/bin/coffee")
    )
  :mode (("\\.coffee$" . coffee-mode)
         ("Cakefile$" . coffee-mode)))

;;----------------------------------------------------------------------------
;; Javascript with js-mode
;;----------------------------------------------------------------------------

(use-package js-mode
  :mode ("\\.js$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2))))
  :config
  (setq js-indent-level 2))


;;----------------------------------------------------------------------------
;; JSON with json-mode
;;----------------------------------------------------------------------------

(use-package json-mode
  :mode ("\\.json$" . json-mode))

(use-package vue-mode
  :mode (("\\.vue\\'" . vue-mode))
  )

(provide 'config-webdev)
