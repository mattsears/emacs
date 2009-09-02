
;; Ruby mode
(require 'ruby-mode)

;; Rspec mode
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(require 'rspec-mode)

(add-hook 'rspec-mode-hook
          '(lambda ()
             (setq yas/mode-symbol 'rspec-mode)))

;; Cucumber
(add-to-list 'load-path "~/.emacs.d/vendor/cucumber-mode")
(require 'cucumber-mode)
(autoload 'feature-mode "feature-mode" "Mode for editing cucumber files" t)
;;(add-to-list 'feature-mode '("\.feature$" . feature-mode))

;; Ruby code
(require 'inf-ruby)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; Rinari
(vendor 'rinari)
(require 'rinari)
(setq rinari-tags-file-name "TAGS")
(add-hook 'rinari-minor-mode-hook
          (lambda ()
            (define-key rinari-minor-mode-map (kbd "A-r") 'rinari-test)
            (define-key rinari-minor-mode-map (kbd "C-c s") 'rinari-find-rspec)
            (define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
            (define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
            (define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)
            ))

;; Rinari (Minor Mode for Ruby On Rails)
(setq rinari-major-modes
      (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
            'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml/")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda ()
            (rinari-launch)
            (setq wrap-region-tag-active t)
            (wrap-region-mode t)
            ))

;; ruby related file types
(setq auto-mode-alist (cons '("\\.html\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . rhtml-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ru$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))

;; RI mode for ruby docs
(add-to-list 'load-path "~/.emacs.d/vendor/ri-emacs")
(setq ri-ruby-script (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-emacs.rb"))
(autoload 'ri "ri-ruby" "Ri mode" t)

;; Ruby hacks
(vendor 'ruby-hacks)

(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-electric)
            (require 'ruby-style)
            (require 'rcodetools)
            (ruby-electric-mode t)
            (require 'ruby-compilation)
            (coding-hook)
            (set-pairs '("(" "{" "[" "\"" "\'" "|"))
            (inf-ruby-keys)
            (wrap-region-mode t)
            (flymake-mode-on)
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace))))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (define-key ruby-mode-map [return] 'newline-and-indent)
            (define-key ruby-mode-map (kbd "C-c s") 'rspec-toggle-spec-and-target)
            (define-key ruby-mode-map (kbd "A-i") 'beautify-ruby)
            (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
            (define-key ruby-mode-map "\C-l" 'ruby-electric-hashrocket)))


(defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
  (let ((comp-buffer-name (format "*%s*" name)))
    (when (get-buffer comp-buffer-name)
      (kill-buffer comp-buffer-name))))
(ad-activate 'ruby-do-run-w/compilation)

;; Treetop
(vendor 'treetop)

;; Ruby debugging.
(setq ruby-dbg-flags "") ;; no warnings when running with compilation
(autoload 'rdebug "rdebug" "Ruby debugging support." t)
(global-set-key [f9] 'gud-step)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-cont)
(global-set-key "\C-c\C-d" 'rdebug)

;; enhance font colors
(font-lock-add-keywords
 'ruby-mode
 '(("\\<\\(private\\)" 1 font-lock-function-name-face t)))

;; Like c-in-literal, only for Ruby
(defun ruby-in-literal ()
  (let* ((here (point))
         (state (save-excursion
                  (ruby-beginning-of-defun)
                  (parse-partial-sexp (point) here))))
    (or (nth 3 state)
        (nth 4 state)
        nil)))

;; Like c-electric-backspace, only for Ruby
(defun ruby-electric-backspace (arg)
  (interactive "*P")
  (if (or arg (ruby-in-literal))
      (backward-delete-char-untabify (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify 1)))))

(defun ruby-electric-delete (arg)
  (interactive "*P")
  (if (or arg (ruby-in-literal))
      (backward-delete-char-untabify (- (prefix-numeric-value arg)))
    (let ((here (point)))
      (skip-chars-forward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify -1)))))

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

;; Redefine this ruby-electric function so that we can use
;; ruby-electric-space w/o the minor mode (which doesn't play nice w/
;; multiple major modes).
(defun ruby-electric-code-at-point-p()
  (let* ((properties (text-properties-at (point))))
    (and (null (memq 'font-lock-string-face properties))
         (null (memq 'font-lock-comment-face properties)))))

(defun ruby-reindent-then-newline-and-indent ()
  (interactive "*")
  (newline)
  (save-excursion
    (end-of-line 0)
    (indent-according-to-mode)
    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
  (indent-according-to-mode))

(defun beautify-ruby ()
  "Run Ruby Beatify script on current region."
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (command "~/bin/beautify"))
    (shell-command-on-region start end command t t
                             shell-command-default-error-buffer)))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

(provide 'ruby)

