(add-to-list 'load-path "~/.emacs.d/vendor")

;; Set up package system
(defvar my-packages
  '(clojure-mode clojure-test-mode nrepl paredit parenface dired-details flycheck)
  "A list of packages to ensure are installed at launch.")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Loat Aquamacs customizations
(setq custom-file "~/.emacs.d/mattsears/custom.el")
(load custom-file)

(load "mattsears/global")
(load "mattsears/bindings")
(load "mattsears/defuns")
(load "mattsears/snippets")
(load "mattsears/modes")
(load "mattsears/theme")
(load "mattsears/local")

(vendor 'treetop)
(vendor 'css)
(vendor 'full-ack)
(vendor 'open-file-in-github)
(smex-initialize)
