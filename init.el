(add-to-list 'load-path "~/.emacs.d/vendor")
(push "/usr/local/bin" exec-path)

;; Set up package system
(defvar my-packages
  '(clojure-mode clojure-test-mode nrepl paredit parenface
                 dired-details flycheck gist magit smartparens
                 exec-path-from-shell multi-term yasnippet
                 coffee-mode rvm smex haml-mode yaml-mode
                 dash rspec-mode sass-mode simplenote multiple-cursors
                 diminish markdown-mode ruby-compilation feature-mode
                 fill-column-indicator sr-speedbar textmate popup mustache-mode
                 fuzzy color-theme ibuffer-vc tabbar github-browse-file
                 git-gutter powerline iedit rbenv sprintly-mode expand-region
                 rainbow-delimiters duplicate-thing httpcode popup ag)
  "A list of packages to ensure are installed at launch.")

(require 'package)

(custom-set-variables
 '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                      ("elpa" . "http://tromey.com/elpa/")
                      ("melpa" . "http://melpa.milkbox.net/packages/")
                      ("marmalade" .
                       "http://marmalade-repo.org/packages/"))))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load "mattsears/global")
(load "mattsears/defuns")
(load "mattsears/local")
(load "mattsears/snippets")
(load "mattsears/modes")
(load "mattsears/theme")
(load "mattsears/bindings")

(vendor 'open-file-in-github)
(smex-initialize)


(fset 'rails3
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 134217848 114 101 return 119 114 105 116 101 95 97 116 116 114 105 98 117 116 101 return 114 97 119 95 119 114 105 116 101 95 97 116 116 114 105 98 117 116 101 return 134217848 return 101 114 114 111 114 115 46 111 110 return 101 114 114 111 114 115 95 111 110 39 backspace return 134217788] 0 "%d")) arg)))
