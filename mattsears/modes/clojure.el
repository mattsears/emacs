;;----------------------------------------------------------------------------
;; Clojure mode
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode")
(setq inferior-lisp-program "~/bin/clojure")
(require  'clojure-auto)
(require 'clojure-paredit)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;;----------------------------------------------------------------------------
;; Slime
;;----------------------------------------------------------------------------
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/vendor/slime")
(require 'slime)
(slime-setup '(slime-fancy slime-asdf))

;;----------------------------------------------------------------------------
;; Swank
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendor/swank-clojure")
(setq swank-clojure-binary "~/bin/clojure")
(require  'swank-clojure-autoload)

(defun clojure ()
  "Starts clojure in Slime"
  (interactive)
  (slime 'clojure))

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

;; add hooks for modes you want to use the tab completion for:
(add-hook 'clojure-mode-hook    'my-tab-fix)
