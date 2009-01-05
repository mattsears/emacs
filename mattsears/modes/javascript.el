; js2
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-auto-indent-flag nil)
(setq js2-bounce-indent-flag t)
(setq js2-enter-indents-newline t)
(setq js2-strict-missing-semi-warning nil)

(add-hook 'js2-mode-hook
    '(lambda ()
       (setq js2-basic-offset 4)
       (setq js2-use-font-lock-faces t)))

; js-shell
(autoload 'javascript-shell "javascript-mode" nil t)

(defun js2-insert-console ()
  (interactive)
  (insert "console.log()")
  (backward-char))

(defun js2-execute-buffer () 
  (interactive)
  (shell-command (concat "johnson " (buffer-file-name))))

(defun js2-mode-match-curly ()
  "Insert matching curly-brace."
  (interactive)
  (insert "{")
  (unless (or (not (looking-at "\\s-*$"))
              (js2-mode-inside-comment-or-string))
    (insert "\n")
    (js2-indent-line)
    (save-excursion
      (insert "\n}")
      (let ((js2-bounce-indent-flag nil))
        (js2-indent-line)))))

(add-hook 'js2-mode-hook '(lambda () 
                            (define-key js2-mode-map (kbd "A-r") 'js2-execute-buffer)
                            (define-key js2-mode-map (kbd "A-R") 'js2-execute-line)
                            (define-key js2-mode-map "\C-T" 'js2-insert-console)
                            
                            (defun js-continued-var-decl-list-p ()
                              "Return non-nil if point is inside a continued variable declaration
list."
                              (interactive)
                              (let ((start (save-excursion (js-re-search-backward "\\<var\\>" nil t))))
                                (and start
                                     (save-excursion (re-search-backward "\n" start t))
                                     (not (save-excursion 
                                            (js-re-search-backward 
                                             ";\\|[^, \t][ \t]*\\(/[/*]\\|$\\)" start t))))))
                            (defun js-proper-indentation (parse-status)
                              "Return the proper indentation for the current line."
                              (save-excursion
                                (back-to-indentation)
                                (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
                                      (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
                                      (continued-expr-p (js-continued-expression-p)))
                                  (cond (ctrl-stmt-indent)
                                        ((js-continued-var-decl-list-p)
                                         (js-re-search-backward "\\<var\\>" nil t)
                                         (+ (current-indentation) js2-basic-offset))
                                        ((nth 1 parse-status)
                                         (goto-char (nth 1 parse-status))
                                         (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                                             (progn
                                               (skip-syntax-backward " ")
                                               (when (= (char-before) ?\)) (backward-list))
                                               (back-to-indentation)
                                               (cond (same-indent-p
                                                      (current-column))
                                                     (continued-expr-p
                                                      (+ (current-column) (* 2 js2-basic-offset)))
                                                     (t
                                                      (+ (current-column) js2-basic-offset))))
                                           (unless same-indent-p
                                             (forward-char)
                                             (skip-chars-forward " \t"))
                                           (current-column)))
                                        (continued-expr-p js2-basic-offset)
                                        (t 0)))))))
