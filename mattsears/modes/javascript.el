;;----------------------------------------------------------------------------
;; Espresso-mode  for editing Javascript files
;;----------------------------------------------------------------------------
(defvar preferred-mmm-javascript-mode 'espresso-mode)
(autoload 'espresso-mode "espresso" "An enhanced version of javascript-mode")
(setq espresso-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(autoload 'javascript-mode "javascript" nil t)
(setq js-indent-level 2)

;; MMM submode regions in html
(eval-after-load "mmm-vars"
  `(progn
     (mmm-add-group
      'html-js
      '((js-script-cdata
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
         :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-script
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t]*\n?"
         :back "[ \t]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-inline
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "on\w+=\""
         :back "\"")))
     (dolist (mode (list 'html-mode 'rhtml-mode))
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'html-js))))

(defun js-insert-console ()
 (interactive)
 (insert "console.log()")
 (backward-char))

(add-hook 'js-mode
          '(lambda ()
             (add-hook 'before-save-hook 'delete-trailing-whitespace)
             (define-key espresso-mode-map "\C-L" 'js-insert-console) ))
            