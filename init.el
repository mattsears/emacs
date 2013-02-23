(add-to-list 'load-path "~/.emacs.d/vendor")

;; Set up package system
(defvar my-packages
  '(clojure-mode clojure-test-mode nrepl paredit parenface
                 dired-details flycheck gist magit smartparens
                 exec-path-from-shell multi-term yasnippet
                 coffee-mode rvm smex haml-mode
                 dash rspec-mode sass-mode simplenote
                 diminish markdown-mode ruby-tools ruby-end
                 ruby-compilation ruby-block feature-mode
                 full-ack nyan-mode fill-column-indicator
                 sr-speedbar textmate popup mustache-mode
                 key-chord)
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

(vendor 'open-file-in-github)
(smex-initialize)
