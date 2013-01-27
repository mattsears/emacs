;;----------------------------------------------------------------------------
;; Clojure mode
;;----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

;; add hooks for modes you want to use the tab completion for:
(add-hook 'clojure-mode-hook    'my-tab-fix)
