;; Flyspell.el Spell checker

(require 'flyspell)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'yaml-mode-hook 'turn-on-flyspell)
(add-hook 'shell-mode-hook 'turn-on-flyspell)
(add-hook 'haml-mode-hook 'turn-on-flyspell)
(add-hook 'markdown-mode-hook 'turn-on-flyspell)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))
