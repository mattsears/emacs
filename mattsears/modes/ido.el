;; ido.el - custom ido settings
(require 'ido)
(setq ido-mode t)
(setq ido-save-directory-list-file nil)
;(setq ido-save-directory-list-file "~/.ido.last")
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(setq ido-decorations (quote (" (" ")" "  " " ..." " [" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]")))
(setq ido-default-buffer-method (quote selected-window))
(setq ido-default-file-method (quote selected-window))
(setq ido-enable-tramp-completion t)
(setq ido-everywhere nil)
(setq ido-use-filename-at-point nil)
(setq ido-use-url-at-point nil)
(setq ido-max-prospects 10)
(setq ido-enable-prefix nil)
(setq ido-confirm-unique-completion t)
(setq ido-show-dot-for-dired t)
(setq ido-work-directory-list '("~/" "~/Desktop" "~/Documents"))
(setq ido-ignore-buffers
   '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido" "^\*.ido.last"))
(setq ido-ignore-directories
   '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./" ".DS_Store"))
(setq ido-ignore-files
   '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".ido.last" ".DS_Store"))

(setq ido-execute-command-cache nil)
 (defun ido-execute-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (progn
        (unless ido-execute-command-cache
          (mapatoms (lambda (s)
                      (when (commandp s)
                        (setq ido-execute-command-cache
                              (cons (format "%S" s) ido-execute-command-cache))))))
        ido-execute-command-cache)))))

;; This tab override shouldn't be necessary given ido's default
;; configuration, but minibuffer-complete otherwise dominates the
;; tab binding because of my custom tab-completion-everywhere
;; configuration.
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

(provide 'ido)
