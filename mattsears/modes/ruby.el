(require 'ruby-mode)

(eval-after-load 'ruby-mode
  '(progn
     (require 'ruby-compilation)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-r") 'ruby-compilation-this-buffer)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")))

;; Set custom flags when running the ruby command in mode-compile
(setq ruby-dbg-flags "-W0")

;; RVM
(add-to-list 'load-path "~/.emacs.d/vendor/rvm.el")
(require 'rvm)

(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

;; A few formatting options
(setq ruby-deep-indent-paren-style nil)
(setq ruby-deep-arglist nil)
(setq ruby-dbg-flags "-W0")

;; RSense
(setq rsense-home "/usr/local/lib/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-rsense)
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

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

;; (add-hook 'ruby-mode-hook 'coding-hook)

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

;;----------------------------------------------------------------------------
;; Ruby - haml & sass
;;----------------------------------------------------------------------------
(vendor 'sass-mode)
(require 'sass-mode)
(require 'haml-mode)
(setq auto-mode-alist (cons '("\\.haml$" . haml-mode) auto-mode-alist))

(vendor 'ruby-hacks)

;;----------------------------------------------------------------------------
;; Ruby Block Helpers
;;----------------------------------------------------------------------------
(vendor 'ruby-block)
(require 'ruby-block)
(ruby-block-mode t)

(setq ruby-block-highlight-toggle t)

;;----------------------------------------------------------------------------
;; Automatically insert 'end' for blocks
;;----------------------------------------------------------------------------

(vendor 'ruby-end)
(require 'ruby-end)

;;----------------------------------------------------------------------------
;; Ruby related file types
;;----------------------------------------------------------------------------
(setq auto-mode-alist (cons '("Rakefile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Guardfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ru$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rabl$" . ruby-mode) auto-mode-alist))
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
