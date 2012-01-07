;; Git goodness
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)
(autoload 'magit-status "magit" nil t)