;; Ruby mode
(require 'ruby-mode)

;; Automatically insert compeleted tags, don't really like it that much
(require 'ruby-electric)

;; Ruby code
(require 'inf-ruby)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

;; File types
(setq auto-mode-alist (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".rake$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))

; Rinari
(vendor 'rinari)
(setq rinari-tags-file-name "TAGS")
(add-hook 'rinari-minor-mode-hook 
          (lambda ()
            (define-key rinari-minor-mode-map (kbd "A-r") 'rinari-test)))

;; RI mode for ruby docs
(add-to-list 'load-path "~/.emacs.d/vendor/ri-emacs")
(setq ri-ruby-script (expand-file-name "~/.emacs.d/vendor/ri-emacs/ri-emacs.rb"))
(autoload 'ri "ri-ruby" "Ri mode" t)

;; Rails
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-rails/")
(require 'rails)

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
		
;; Continuation lines should be indented. 
(defadvice ruby-calculate-indent
  (after ruby-indent-continuation-lines activate)
  "Advise ruby-mode to further indent continuation lines."
  (save-excursion
    (goto-char (point-at-bol))
    (skip-chars-backward " \t\n")
    (when (eq ?\\ (char-before))
      (setq ad-return-value (+ ruby-indent-level ad-return-value)))))

;; Alignment
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))


;; Ruby hookers
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys) ))
(add-hook 'ruby-mode-hook '(lambda() (local-set-key "\r" 'ruby-reindent-then-newline-and-indent)))
(add-hook 'ruby-mode-hook '(lambda () 
	(if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
     				(flymake-mode))	))
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
            (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
			(define-key ruby-mode-map "\C-l" 'ruby-electric-hashrocket)	
            (require 'ruby-electric)
            (ruby-electric-mode t)))
 
(defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
  (let ((comp-buffer-name (format "*%s*" name)))
    (when (get-buffer comp-buffer-name)
      (kill-buffer comp-buffer-name))))
(ad-activate 'ruby-do-run-w/compilation)

;; Treetop
(vendor 'treetop)

(provide 'ruby)
