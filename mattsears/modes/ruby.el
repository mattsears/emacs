(require 'ruby-mode)

(eval-after-load 'ruby-mode
  '(progn
     (require 'ruby-compilation)
     ;;(add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (define-key ruby-mode-map (kbd "RET") 'ruby-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")))

;;----------------------------------------------------------------------------
;; Ruby - Testing
;;----------------------------------------------------------------------------

;; Clear the compilation buffer between test runs.
(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

(add-hook 'ruby-mode-hook 'coding-hook)

;; RSpec
(vendor 'rspec-mode)
(add-to-list 'auto-mode-alist '("_spec.rb$" . rspec-mode))
(require 'rspec-mode)

;; Shoulda
(vendor 'shoulda-mode)
(add-to-list 'auto-mode-alist '("_test.rb$" . shoulda-mode))
(require 'shoulda-mode)

;; Cucumber
(add-to-list 'load-path "~/.emacs.d/vendor/cucumber")
(require 'feature-mode)

;; RVM
(add-to-list 'load-path "~/.emacs.d/vendor/rvm.el")
(require 'rvm)

;;----------------------------------------------------------------------------
;; Ruby - haml & sass
;;----------------------------------------------------------------------------
(require 'sass-mode)
(require 'haml-mode)

(setq auto-mode-alist (cons '("\\.haml$" . haml-mode) auto-mode-alist))

(vendor 'ruby-hacks)

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

(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (flymake-mode)
					  (add-hook 'local-write-file-hooks
					    '(lambda()
					      (save-excursion
					        (untabify (point-min) (point-max))
					        (delete-trailing-whitespace))))
                      )))

;;----------------------------------------------------------------------------
;; Ruby related functions
;;----------------------------------------------------------------------------

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

(defun ruby-module-path (module)
  (shell-command-to-string
   (concat
    "ruby -e "
    "\"ret='()';$LOAD_PATH.each{|p| "
    "x=p+'/'+ARGV[0].gsub('.rb', '')+'.rb';"
    "ret=File.expand_path(x)"
    "if(File.exist?(x))};printf ret\" "
    module)))

(provide 'ruby)
