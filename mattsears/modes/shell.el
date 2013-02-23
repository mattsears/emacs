;;----------------------------------------------------------------------------
;; Shell customizations
;;----------------------------------------------------------------------------


(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/bash")   ;; use bash

;; only needed if you use autopair
(add-hook 'term-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))


(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; set emacs path
;; (setq path "/usr/bin;/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:~/dotfiles/bin:~/Workspace/taco/bin::/usr/local/pgsql/bin")
;; (setenv "PATH" path)
;; (push "/usr/local/bin" exec-path)

;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;; (setenv "PATH" (concat "$HOME/.rbenv/bin" ":" (getenv "PATH")))

;; colorful shell
;; (require 'ansi-color)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))

;; (setq comint-prompt-read-only t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (eval-after-load 'shell
;;   '(progn
;;      (define-key shell-mode-map [up] 'comint-previous-input)
;;      (define-key shell-mode-map [down] 'comint-next-input)
;;      (define-key shell-mode-map "\C-p" 'comint-previous-input)
;;      (define-key shell-mode-map "\C-n" 'comint-next-input)))

;; (setq eshell-cmpl-cycle-completions nil
;;       eshell-save-history-on-exit t
;;       eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;; (eval-after-load 'esh-opt
;;   '(progn
;;      (require 'em-prompt)
;;      (require 'em-term)
;;      (require 'em-cmpl)
;;      (setenv "PAGER" "cat")
;;      (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
;;          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
;;      (add-to-list 'eshell-visual-commands "ssh")
;;      (add-to-list 'eshell-visual-commands "tail")
;;      (add-to-list 'eshell-command-completions-alist
;;                   '("gunzip" "gz\\'"))
;;      (add-to-list 'eshell-command-completions-alist
;;                   '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
;;      (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))
