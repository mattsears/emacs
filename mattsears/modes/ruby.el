
;; Ruby mode
(require 'ruby-mode)

;; Rspec mode
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(require 'rspec-mode)

;; Cucumber
(add-to-list 'load-path "~/.emacs.d/vendor/cucumber-mode")
(require 'cucumber-mode)
(autoload 'feature-mode "feature-mode" "Mode for editing cucumber files" t)
;;(add-to-list 'feature-mode '("\.feature$" . feature-mode))

;; Ruby code
(require 'inf-ruby)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(defun run-ruby-in-buffer (buf script &optional params)
  "Run CMD as a ruby process in BUF if BUF does not exist."
  (let ((abuf (concat "*" buf "*")))
    (when (not (comint-check-proc abuf))
      (set-buffer (make-comint buf rails-ruby-command nil script params)))
    (inferior-ruby-mode)
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern "^>> "
          inferior-ruby-prompt-pattern "^>> ")
    (pop-to-buffer abuf)))

(defun complete-ruby-method (prefix &optional maxnum)
  (if (capital-word-p prefix)
      (let* ((cmd "x = []; ObjectSpace.each_object(Class){|i| x << i.to_s}; x.map{|i| i.match(/^%s/) ? i.gsub(/^%s/, '') : nil }.compact.sort{|x,y| x.size <=> y.size}") ;
             (cmd (if maxnum (concat cmd (format "[0...%s]" maxnum)) cmd)))
        (el4r-ruby-eval (format cmd prefix prefix)))
    (save-excursion
      (goto-char (- (point) (+ 1 (length prefix))))
      (when (and (looking-at "\\.")
                 (capital-word-p (word-at-point))
                 (el4r-ruby-eval (format "::%s rescue nil" (word-at-point))))
        (let* ((cmd "%s.public_methods.map{|i| i.match(/^%s/) ? i.gsub(/^%s/, '') : nil }.compact.sort{|x,y| x.size <=> y.size}")
               (cmd (if maxnum (concat cmd (format "[0...%s]" maxnum)) cmd)))
          (el4r-ruby-eval (format cmd (word-at-point) prefix prefix)))))))


;; File types
(setq auto-mode-alist (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".ru$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".rake$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; Rinari
(vendor 'rinari)
(require 'rinari)
(setq rinari-tags-file-name "TAGS")
(add-hook 'rinari-minor-mode-hook
          (lambda ()
            (define-key rinari-minor-mode-map (kbd "A-r") 'rinari-test)))

(define-key rinari-minor-mode-map [(control meta shift down)] 'rinari-find-rspec)
(define-key rinari-minor-mode-map [(control meta shift left)] 'rinari-find-controller)
(define-key rinari-minor-mode-map [(control meta shift up)] 'rinari-find-model)
(define-key rinari-minor-mode-map [(control meta shift right)] 'rinari-find-view)

;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml/")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

(setq auto-mode-alist (cons '("\\.html\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . rhtml-mode) auto-mode-alist))

;; RI mode for ruby docs
(add-to-list 'load-path "~/.emacs.d/vendor/ri-emacs")
(setq ri-ruby-script (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-emacs.rb"))
(autoload 'ri "ri-ruby" "Ri mode" t)

;; Rails
;;(add-to-list 'load-path "~/.emacs.d/vendor/emacs-rails/")
;;(require 'rails)
;;(require 'rails-lib)
;;(require 'rails-ui)

;; Ruby hacks
(vendor 'ruby-hacks)

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

;; Redefine this ruby-electric function so that we can use
;; ruby-electric-space w/o the minor mode (which doesn't play nice w/
;; multiple major modes).
(defun ruby-electric-code-at-point-p()
  (let* ((properties (text-properties-at (point))))
    (and (null (memq 'font-lock-string-face properties))
         (null (memq 'font-lock-comment-face properties)))))

(defun ruby-electric-hashrocket ()
  "Insert a hash rocket"
  (interactive)
  (insert " => "))

; rinari
(vendor 'rinari)
(setq rinari-tags-file-name "TAGS")
(add-hook 'rinari-minor-mode-hook
          (lambda ()
            (define-key rinari-minor-mode-map (kbd "A-r") 'rinari-test)))

; ruby
(vendor 'ruby-hacks)
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))

;; no warnings when compiling
(setq ruby-dbg-flags "")

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace))))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
            (require 'ruby-electric)
            (ruby-electric-mode t)))

(defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
  (let ((comp-buffer-name (format "*%s*" name)))
    (when (get-buffer comp-buffer-name)
      (kill-buffer comp-buffer-name))))
(ad-activate 'ruby-do-run-w/compilation)

(defun ruby-reindent-then-newline-and-indent ()
  (interactive "*")
  (newline)
  (save-excursion
    (end-of-line 0)
    (indent-according-to-mode)
    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
  (indent-according-to-mode))

;; Ruby hookers
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys) ))
(add-hook 'ruby-mode-hook '(lambda() (local-set-key "\r" 'ruby-reindent-then-newline-and-indent)))
(add-hook 'ruby-mode-hook '(lambda ()
                             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                                 (flymake-mode))  ))

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace))))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 4)
            (define-key ruby-mode-map [return] 'newline-and-indent)
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
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
(autoload 'rdebug "rdebug" "Ruby debugging support." t)
(global-set-key [f9] 'gud-step)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-cont)
(global-set-key "\C-c\C-d" 'rdebug)

(defun beautify-ruby ()
  "Run Ruby Beatify script on current region."
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (command "~/bin/beautify"))
        (shell-command-on-region start end command t t
             shell-command-default-error-buffer)))

(provide 'ruby)

