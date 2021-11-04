
;; Loads $PATH when Emacs is opened via gui
(push "/usr/local/bin" exec-path)

(defun copy-shell-environment-variables ()
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell
  :init
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
    ))

(provide 'config-shell)
