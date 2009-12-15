;;----------------------------------------------------------------------------
;; Initialize individual mode customizations
;;----------------------------------------------------------------------------

(load "~/.emacs.d/mattsears/modes/snippets")
(load "~/.emacs.d/mattsears/modes/css")
(load "~/.emacs.d/mattsears/modes/dired")
(load "~/.emacs.d/mattsears/modes/javascript")
(load "~/.emacs.d/mattsears/modes/ruby")
(load "~/.emacs.d/mattsears/modes/rails")
(load "~/.emacs.d/mattsears/modes/ido")
(load "~/.emacs.d/mattsears/modes/clojure")
(load "~/.emacs.d/mattsears/modes/markdown")
(load "~/.emacs.d/mattsears/modes/buffer")
(load "~/.emacs.d/mattsears/modes/org")
(load "~/.emacs.d/mattsears/modes/erc")
(load "~/.emacs.d/mattsears/modes/shell")
;;(load "~/.emacs.d/mattsears/modes/autocomplete")
(load "~/.emacs.d/mattsears/modes/anything")

;; Wrap words in text-mode
(autoload 'longlines-mode "longlines.el" "Minor mode for editing long lines." t)
(add-hook 'text-mode-hook 'longlines-mode)

;; Other text mode improvements
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (auto-fill-mode 1)
             (column-marker-1 80)
             ))

;; Bash
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))

;; Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key (kbd "C-c c") 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c k") 'mode-compile-kill)
;; Set custom flags when running the ruby command in mode-compile
(setq ruby-dbg-flags nil)

;; Indicate syntax errors
(add-to-list 'load-path "~/.emacs.d/vendor/flymake.el")
(require 'flymake)

;; Yaml
(add-to-list 'load-path "~/.emacs.d/vendor/yaml")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Git goodness
(require 'magit)
(autoload 'magit-status "magit" nil t)

;; Midnight mode to clean up old buffers
(require 'midnight)

;; Spell checker
(add-to-list 'load-path "~/.emacs.d/vendor/flyspell.el")
(require 'flyspell)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

(add-to-list 'load-path "~/.emacs.d/vendor/zencoding-mode.el")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(add-hook 'rhtml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(provide 'modes)

