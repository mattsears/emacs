(require 'ruby-mode)

;;----------------------------------------------------------------------------
;; Ruby - Rspec
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(require 'rspec-mode)
(add-hook 'rspec-mode-hook
          '(lambda ()
             (setq yas/mode-symbol 'rspec-mode)))

;;----------------------------------------------------------------------------
;; Ruby - Cucumber
;;----------------------------------------------------------------------------
(require 'feature-mode)
;;(add-to-list 'feature-mode '("\.feature$" . feature-mode)) ;; Doesn't work!

;;----------------------------------------------------------------------------
;; Ruby - haml & sass
;;----------------------------------------------------------------------------
(require 'sass-mode)
(require 'haml-mode)

;;----------------------------------------------------------------------------
;; Ruby - Inferior Mode
;;----------------------------------------------------------------------------
(require 'inf-ruby)
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;;----------------------------------------------------------------------------
;; Ruby - Electric mode
;;----------------------------------------------------------------------------
(autoload 'ruby-electric-mode "ruby-electric"
  "Electric brackes/quotes/keywords for Ruby source" t)

;; Only use ruby-electric for adding 'end'
(setq ruby-electric-expand-delimiters-list nil)
(add-hook 'ruby-mode-hook
          (lambda () (ruby-electric-mode t)))

;;----------------------------------------------------------------------------
;; Ruby - RHTML Mode
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml/")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda ()
            (setq wrap-region-tag-active t)
            (define-key rhtml-mode-map "\C-RET" 'zencoding-expand-line)
            ;;(wrap-region-mode t)
            ))
(setq auto-mode-alist (cons '("\\.html\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.liquidl$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . rhtml-mode) auto-mode-alist))

;; Hack: Add color to angle brackets and strings in html
(defvar html-mode-keywords
  '(("\\(<[^>]+>\\)" 1 font-lock-variable-name-face prepend)
    ("\\(\"[^\"]*\"\\)" 1 font-lock-string-face prepend)
    ("\\('[^']*'\\)" 1 font-lock-string-face prepend)))

(font-lock-add-keywords 'rhtml-mode html-mode-keywords)
(font-lock-add-keywords 'html-mode html-mode-keywords)
(font-lock-add-keywords 'html-helper-mode html-mode-keywords)

;; Hack: Add color to strings in ERB
(add-to-list 'rhtml-in-erb-keywords '("\\(#{[^>]*}\\)" .
                                      (1 font-lock-doc-face prepend)) )
(add-to-list 'rhtml-in-erb-keywords '("\\(<!--[^>]*-->\\)" .
                                      (1 font-lock-comment-face prepend)) )
(add-to-list 'rhtml-in-erb-keywords '("\\(\"[^>]*\"\\)" .
                                      (1 font-lock-string-face prepend)) )
(add-to-list 'rhtml-in-erb-keywords '("\\(\'[^>]*\'\\)" .
                                      (1 font-lock-string-face prepend)) )

;;----------------------------------------------------------------------------
;; Ruby related file types
;;----------------------------------------------------------------------------
(setq auto-mode-alist (cons '("Rakefile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ru$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.gemspec$" . ruby-mode) auto-mode-alist))

;;----------------------------------------------------------------------------
;; Ruby - RI mode for ruby docs
;;----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/vendor/ri-emacs")
(setq ri-ruby-script (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-emacs.rb"))
(autoload 'ri "ri-ruby" "Ri mode" t)

(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-style)
            (require 'rcodetools)
            (require 'ruby-compilation)
            (require 'rspec-mode)
            (ruby-electric-mode t)
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace))))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
            (define-key ruby-mode-map [return] 'ruby-newline-and-indent)
            (define-key ruby-mode-map (kbd "C-c s") 'rspec-toggle-spec-and-target)
            (define-key ruby-mode-map (kbd "C-m") 'ruby-reindent-then-newline-and-indent)))

;;----------------------------------------------------------------------------
;; Ruby - Syntax checking
;;----------------------------------------------------------------------------
(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))

(defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
  (let ((comp-buffer-name (format "*%s*" name)))
    (when (get-buffer comp-buffer-name)
      (kill-buffer comp-buffer-name))))
(ad-activate 'ruby-do-run-w/compilation)

;;----------------------------------------------------------------------------
;; Ruby - debugging
;;----------------------------------------------------------------------------
(setq ruby-dbg-flags "") ;; no warnings when running with compilation
(autoload 'rdebug "rdebug" "Ruby debugging support." t)
(global-set-key [f9] 'gud-step)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-cont)
(global-set-key "\C-c\C-d" 'rdebug)

;;----------------------------------------------------------------------------
;; Ruby related functions
;;----------------------------------------------------------------------------

;; Code borrowed from Emacs starter kit
(defun rr (&optional arg)
  "Run a Ruby interactive shell session in a buffer."
  (interactive "P")
  (let ((impl (if (not arg)
                  "mri"
                (completing-read "Ruby Implementation: "
                                 '("ruby" "jruby" "rubinius" "yarv")))))
    (run-ruby (cdr (assoc impl '(("mri" . "irb")
                                 ("jruby" . "jruby -S irb")
                                 ("rubinius" . "rbx")
                                 ("yarv" . "irb1.9")))))
    (with-current-buffer "*ruby*"
      (rename-buffer (format "*%s*" impl) t))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

(defun ruby-reindent-then-newline-and-indent ()
  "Reindents the current line then creates an indented newline."
  (interactive "*")
  (newline)
  (save-excursion
    (end-of-line 0)
    (indent-according-to-mode)
    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
  (when (ruby-previous-line-is-comment)
    (insert "# "))
  (indent-according-to-mode))

(defun ruby-previous-line-is-comment ()
  "Returns `t' if the previous line is a Ruby comment."
  (save-excursion
    (forward-line -1)
    (ruby-line-is-comment)))

(defun ruby-line-is-comment ()
  "Returns `t' if the current line is a Ruby comment."
  (save-excursion
    (beginning-of-line)
    (search-forward "#" (point-at-eol) t)))

;;----------------------------------------------------------------------------
;; Ruby - Misc.
;;----------------------------------------------------------------------------

;; Ruby hacks
(vendor 'ruby-hacks)

;; Enhance font colors
(font-lock-add-keywords
 'ruby-mode
 '(("\\<\\(private\\)" 1 font-lock-function-name-face t))
 '(("\\<\\(protected\\)" 1 font-lock-function-name-face t)))

(provide 'ruby)
