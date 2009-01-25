;; Global snippets - Using Emacs Abbreviations here instead of yasnippet
(define-abbrev global-abbrev-table "mts" "Matt Sears")
(define-abbrev global-abbrev-table "matt@" "matt@mattsears.com")

(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
