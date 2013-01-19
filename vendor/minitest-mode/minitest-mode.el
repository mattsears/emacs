;;; minitest-mode.el --- Enhance ruby-mode for Minitest

;; Copyright (C) 2008-2011 Peter Williams <http://barelyenough.org>
;; Authors: Peter Williams, et al.
;; URL: http://github.com/pezra/minitest-mode
;; Created: 2011
;; Version: 1.3
;; Keywords: minitest ruby
;; Package-Requires: ((ruby-mode "1.1")
;;                    (mode-compile "2.29"))

;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Documentation:
;;
;; This minor mode provides some enhancements to ruby-mode in
;; the contexts of Minitest specifications.  Namely, it provides the
;; following capabilities:
;;
;;  * toggle back and forth between a spec and it's target (bound to
;;    `\C-c ,t`)
;;
;;  * verify the spec file associated with the current buffer (bound to `\C-c ,v`)
;;
;;  * verify the spec defined in the current buffer if it is a spec
;;    file (bound to `\C-c ,v`)
;;
;;  * verify the example defined at the point of the current buffer (bound to `\C-c ,s`)
;;
;;  * re-run the last verification process (bound to `\C-c ,r`)
;;
;;  * toggle the pendingness of the example at the point (bound to
;;    `\C-c ,d`)
;;
;;  * disable the example at the point by making it pending
;;
;;  * reenable the disabled example at the point
;;
;;  * run spec for entire project (bound to `\C-c ,a`)
;;
;; You can choose whether to run specs using 'rake spec' or the 'spec'
;; command. Use the customization interface (customize-group
;; minitest-mode) or override using (setq minitest-use-rake-flag TVAL).
;;
;; Options will be loaded from spec.opts or .minitest if it exists and
;; minitest-use-opts-file-when-available is not set to nil, otherwise it
;; will fallback to defaults.
;;
;; Dependencies
;; ------------
;;
;; This minor mode depends on `mode-compile`.  The expectations depend
;; `on el-expectataions.el`.  If `ansi-color` is available it will be
;; loaded so that minitest output is colorized properly.  If
;; `minitest-use-rvm` is set to true `rvm.el` is required.
;;

;;; Change Log:
;;
;; 1.5 - Allow key prefix to be customized (`minitest-key-command-prefix`)
;; 1.4 - Allow .minitest/spec.opts files to be ignored (`minitest-use-opts-file-when-available` customization)
;; 1.3 - Bundler support (JD Huntington)
;; 1.2 - Minitest2 compatibility  (Anantha Kumaran)
;; 1.1 - Run verification processes from project root directory (Joe Hirn)
;; 1.0 - Advance to end of compilation buffer even if it not the other window (byplayer)
;; 0.8 - RVM support (Peter Williams)
;; 0.7 - follow RoR conventions for file in lib directory (Tim Harper)
;; 0.6 - support for arbitrary spec and rake commands (David Yeu)
;; 0.5 - minor changes from Tim Harper
;; 0.4 - ansi colorization of compliation buffers (teaforthecat)
;; 0.3 - Dave Nolan implements respect for spec.opts config and
;;       custom option to use 'rake spec' task or 'spec' command
;; 0.2 - Tim Harper implemented support for imenu to generate a basic
;;       tag outline
;; 0.1 - Pezra's version in master

;;; Code:
(require 'ruby-mode)

(defconst minitest-mode-abbrev-table (make-abbrev-table))

(define-prefix-command 'minitest-mode-verifible-keymap)
(define-key minitest-mode-verifible-keymap (kbd "v") 'minitest-verify)
(define-key minitest-mode-verifible-keymap (kbd "a") 'minitest-verify-all)
(define-key minitest-mode-verifible-keymap (kbd "t") 'minitest-toggle-spec-and-target)

(define-prefix-command 'minitest-mode-keymap)
(define-key minitest-mode-keymap (kbd "v") 'minitest-verify)
(define-key minitest-mode-keymap (kbd "a") 'minitest-verify-all)
(define-key minitest-mode-keymap (kbd "t") 'minitest-toggle-spec-and-target)
(define-key minitest-mode-keymap (kbd "s") 'minitest-verify-single)
(define-key minitest-mode-keymap (kbd "d") 'minitest-toggle-example-pendingness)

(defgroup minitest-mode nil
  "Minitest minor mode.")

(defcustom minitest-use-rake-flag t
  "*Whether minitest runner is run using rake spec task or the ruby command"
  :tag "Minitest runner command"
  :type '(radio (const :tag "Use 'rake spec' task" t)
                (const :tag "Use 'spec' command" nil))
  :group 'minitest-mode)

(defcustom minitest-rake-command "rake"
  "The command for rake"
  :type 'string
  :group 'minitest-mode)

(defcustom minitest-spec-command "ruby"
  "The command for spec"
  :type 'string
  :group 'minitest-mode)

(defcustom minitest-use-rvm nil
  "t when RVM in is in use. (Requires rvm.el)"
  :type 'boolean
  :group 'minitest-mode)

(defcustom minitest-use-bundler-when-possible t
  "t when minitest should be run with 'bundle exec' whenever possible. (Gemfile present)"
  :type 'boolean
  :group 'minitest-mode)

(defcustom minitest-use-opts-file-when-available t
  "t if minitest should use .minitest/spec.opts"
  :type 'boolean
  :group 'minitest-mode)

(defcustom minitest-compilation-buffer-name "*compilation*"
  "The compilation buffer name for spec"
  :type 'string
  :group 'minitest-mode)

(defcustom minitest-key-command-prefix  (kbd "C-c ,")
  "The prefix for all minitest related key commands"
  :type 'string
  :group 'minitest-mode)


;;;###autoload
(define-minor-mode minitest-mode
  "Minor mode for minitest files"
  :lighter " minitest"
  (local-set-key minitest-key-command-prefix minitest-mode-keymap))

(defvar minitest-imenu-generic-expression
  '(("Examples"  "^\\( *\\(it\\|describe\\|context\\) +.+\\)"          1))
  "The imenu regex to parse an outline of the minitest file")

(defun minitest-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression minitest-imenu-generic-expression))

(add-hook 'minitest-mode-hook 'minitest-set-imenu-generic-expression)

;; Snippets
(if (require 'snippet nil t)
    (snippet-with-abbrev-table
     'minitest-mode-abbrev-table
     ("helper" . "require 'pathname'\nrequire Pathname(__FILE__).dirname + '../test_helper'\n\n$.")
     ("desc"   . "describe $${ClassName} do\n  $.\nend ")
     ("descm"  . "describe $${ClassName}, \"$${modifier}\" do\n  $.\nend ")
     ("it"     . "it \"should $${what exactly?}\" do\n  $.\n  end ")
     ("bef"    . "before do\n  $.\n  end"))
  )

(defun minitest-beginning-of-example ()
  "Moves point to the beginning of the example in which the point current is."
  (interactive)
  (let ((start (point)))
    (goto-char
     (save-excursion
       (end-of-line)
       (unless (and (search-backward-regexp "^[[:space:]]*it[[:space:]]*(?[\"']" nil t)
                    (save-excursion (ruby-end-of-block) (< start (point))))
         (error "Unable to find an example"))
       (point)))))

(defun minitest-example-pending-p ()
  "True if the example under point is pending. Otherwise false"
  (interactive)
  (save-excursion
    (minitest-beginning-of-example)
    (re-search-forward "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)) t)))


(defun minitest-toggle-example-pendingness ()
  "Disables active examples and enables pending examples."
  (interactive)
  (if (minitest-example-pending-p)
      (minitest-enable-example)
    (minitest-disable-example)))

(defun minitest-disable-example ()
  "Disable the example in which the point is located"
  (interactive)
  (when (not (minitest-example-pending-p))
    (save-excursion
      (minitest-beginning-of-example)
      (end-of-line)
      (insert "\npending")
      (indent-for-tab-command))))

(defun minitest-enable-example ()
  "Enable the example in which the point is located"
  (interactive)
  (when (minitest-example-pending-p)
    (save-excursion
      (minitest-beginning-of-example)
      (search-forward-regexp "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)))
      (beginning-of-line)
      (delete-region (save-excursion (beginning-of-line) (point))
                     (save-excursion (forward-line 1) (point))))))

(defun minitest-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (minitest-run-single-file (minitest-spec-file-for (buffer-file-name)) (minitest-core-options ())))

(defun minitest-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (minitest-run-single-file (minitest-spec-file-for (buffer-file-name)) (minitest-core-options ()) (concat "--line " (number-to-string (line-number-at-pos)))))

(defun minitest-verify-all ()
  "Runs the 'spec' rake task for the project of the current file."
  (interactive)
  (minitest-run (minitest-core-options)))

(defun minitest-toggle-spec-and-target ()
  "Switches to the spec for the current buffer if it is a
   non-spec file, or switch to the target of the current buffer
   if the current is a spec"
  (interactive)
  (find-file
   (if (minitest-buffer-is-spec-p)
       (minitest-target-file-for (buffer-file-name))
     (minitest-spec-file-for (buffer-file-name)))))
(defun minitest-spec-directory-has-lib? (a-file-name)
  (file-directory-p (concat (minitest-spec-directory a-file-name) "/lib")))


(defun minitest-spec-file-for (a-file-name)
  "Find spec for the specified file"
  (if (minitest-spec-file-p a-file-name)
      a-file-name
    (let ((replace-regex (if (and (minitest-target-lib-file-p a-file-name) (minitest-spec-directory-has-lib? a-file-name))
                             "^\\.\\./"
                           "^\\.\\./[^/]+/"))
          (relative-file-name (file-relative-name a-file-name (minitest-spec-directory a-file-name))))
      (minitest-specize-file-name (expand-file-name (replace-regexp-in-string replace-regex "" relative-file-name)
                                                 (minitest-spec-directory a-file-name))))))

(defun minitest-spec-lib-file-p (a-spec-file-name)
  (string-match (concat "^" (expand-file-name (regexp-quote (concat (minitest-spec-directory a-spec-file-name) "/lib")))) a-spec-file-name))

(defun minitest-target-lib-file-p (a-file-name)
  (string-match (concat "^" (expand-file-name (regexp-quote (concat (minitest-project-root a-file-name) "/lib")))) a-file-name))

(defun minitest-target-file-for (a-spec-file-name)
  "Find the target for a-spec-file-name"
  (first
   (file-expand-wildcards
    (replace-regexp-in-string
     "/spec/"
     (if (minitest-spec-lib-file-p a-spec-file-name) "/" "/*/")
     (minitest-targetize-file-name a-spec-file-name)))))

(defun minitest-specize-file-name (a-file-name)
  "Returns a-file-name but converted in to a spec file name"
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string "\\(\\.rb\\)?$" "_spec.rb" (file-name-nondirectory a-file-name))))

(defun minitest-targetize-file-name (a-file-name)
  "Returns a-file-name but converted into a non-spec file name"
  (concat (file-name-directory a-file-name)
          (minitest-file-name-with-default-extension
           (replace-regexp-in-string "_test\\.rb" "" (file-name-nondirectory a-file-name)))))

(defun minitest-file-name-with-default-extension (a-file-name)
  "Adds .rb file extension to a-file-name if it does not already have an extension"
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".rb")))

(defun minitest-directory-subdirectories (directory)
  "Returns list of subdirectories"
  (remove-if
   (lambda (dir) (or (string-match "^\\.\\.?$" (file-name-nondirectory dir))
                     (not (file-directory-p dir))))
   (directory-files directory t)))

(defun minitest-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun minitest-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (minitest-parent-directory a-directory)))

(defun minitest-spec-directory (a-file)
  "Returns the nearest spec directory that could contain specs for a-file"
  (if (file-directory-p a-file)
      (or
       (first (directory-files a-file t "^test$"))
       (if (minitest-root-directory-p a-file)
           nil
         (minitest-spec-directory (minitest-parent-directory a-file))))
    (minitest-spec-directory (minitest-parent-directory a-file))))

(defun minitest-spec-file-p (a-file-name)
  "Returns true if the specified file is a test"
  (numberp (string-match "\\(_\\|-\\)test\\.rb$" a-file-name)))

(defun minitest-core-options (&optional default-options)
  "Returns string of options that instructs spec to use options file if it exists, or sensible defaults otherwise"
  (cond ((and minitest-use-opts-file-when-available
              (file-readable-p (minitest-spec-opts-file)))
         (concat "--options " (minitest-spec-opts-file)))
        (t (or default-options))))

(defun minitest-bundle-p ()
  (and minitest-use-bundler-when-possible
       (file-readable-p (concat (minitest-project-root) "Gemfile"))))

(defun minitest2-p ()
  (or (string-match "minitest" minitest-spec-command)
      (file-readable-p (concat (minitest-project-root) ".minitest"))))

(defun minitest-default-options ()
  (if (minitest2-p)
      "--format documentation"
    (concat "--format specdoc " "--reverse")))

(defun minitest-spec-opts-file ()
  "Returns filename of spec opts file"
  (if (minitest2-p)
      (expand-file-name ".minitest" (minitest-project-root))
    (expand-file-name "spec.opts" (minitest-spec-directory (minitest-project-root)))))

(defun minitest-runner ()
  "Returns command line to run minitest"
  (let ((bundle-command (if (minitest-bundle-p) "bundle exec " "")))
    (concat bundle-command (if minitest-use-rake-flag
                               (concat minitest-rake-command " ruby")
                             minitest-spec-command))))

(defun minitest-runner-options (&optional opts)
  "Returns string of options for command line"
  (let ((opts (if (listp opts)
                  opts
                (list opts))))
    (concat (when minitest-use-rake-flag "SPEC_OPTS=\'")
            (mapconcat 'identity opts " ")
            (when minitest-use-rake-flag "\'"))))

(defun minitest-runner-target (target)
  "Returns target file/directory wrapped in SPEC if using rake"
  (concat (when minitest-use-rake-flag "SPEC=\'") target (when minitest-use-rake-flag "\'")))

;;;###autoload
(defun minitest-buffer-is-spec-p ()
  "Returns true if the current buffer is a spec"
  (and (buffer-file-name)
       (minitest-spec-file-p (buffer-file-name))))

(defun minitest-example-name-at-point ()
  "Returns the name of the example in which the point is currently positioned; or nil if it is outside of and example"
  (save-excursion
    (minitest-beginning-of-example)
    (re-search-forward "it[[:space:]]+['\"]\\(.*\\)['\"][[:space:]]*\\(do\\|DO\\|Do\\|{\\)")
    (match-string 1)))

(defun minitest-end-of-buffer-target-window (buf-name)
  "end of line target window"
  (let ((cur-window (selected-window))
        (com-buffer (get-buffer buf-name)))
    (if com-buffer
        (let ((com-window (get-buffer-window com-buffer)))
          (cond (com-window
                 (unwind-protect
                     (progn
                       (select-window com-window)
                       (with-no-warnings
                         (goto-char (point-max))
                         (recenter '(t))))
                   (select-window cur-window))))))))

(defun minitest-run (&optional opts)
  "Runs spec with the specified options"
  (minitest-compile (minitest-spec-directory (minitest-project-root)) opts))

(defun minitest-run-single-file (spec-file &rest opts)
  "Runs spec on a file with the specified options"
  (minitest-compile (minitest-runner-target spec-file) opts))

(defun minitest-compile (a-file-or-dir &optional opts)
  "Runs a compile for the specified file or diretory with the specified opts"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") (eval `(lambda () (interactive)
                                       (minitest-from-direcory ,default-directory
                                                            (minitest-compile ,a-file-or-dir (quote ,opts))))))
    (global-set-key minitest-key-command-prefix map))

  (if minitest-use-rvm
      (rvm-activate-corresponding-ruby))
  (minitest-from-project-root
   (compile (mapconcat 'identity `(,(minitest-runner) ,a-file-or-dir ,(minitest-runner-options opts)) " ")))
  (minitest-end-of-buffer-target-window minitest-compilation-buffer-name))


(defun minitest-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((minitest-root-directory-p directory)
           (error "Could not determine the project root."))
          ((file-exists-p (expand-file-name "Rakefile" directory)) directory)
          ((file-exists-p (expand-file-name "Gemfile" directory)) directory)
          (t (minitest-project-root (file-name-directory (directory-file-name directory)))))))

(defmacro minitest-from-direcory (directory body-form)
  "Peform body-form from the specified directory"
  `(let ((default-directory ,directory))
     ,body-form))

(defmacro minitest-from-project-root (body-form)
  "Peform body-form from the project root directory"
  `(minitest-from-direcory ,(or (minitest-project-root) default-directory)
                        ,body-form))

;; Makes sure that Minitest buffers are given the minitest minor mode by default
;;;###autoload
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook
             (lambda ()
               (when (minitest-buffer-is-spec-p)
                 (minitest-mode)))))

;; Add verify related spec keybinding to ruby ruby modes
;;;###autoload
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook
             (lambda ()
               (local-set-key minitest-key-command-prefix minitest-mode-verifible-keymap))))

;; Add verify related spec keybinding to ruby ruby modes
;;;###autoload
(eval-after-load 'rails
  '(add-hook 'rails-minor-mode-hook
             (lambda ()
               (local-set-key minitest-key-command-prefix minitest-mode-verifible-keymap))))

;; abbrev
;; from http://www.opensource.apple.com/darwinsource/Current/emacs-59/emacs/lisp/derived.el
(defun merge-abbrev-tables (old new)
  "Merge an old abbrev table into a new one.
This function requires internal knowledge of how abbrev tables work,
presuming that they are obarrays with the abbrev as the symbol, the expansion
as the value of the symbol, and the hook as the function definition."
  (when old
    (mapatoms
     (lambda(it)
       (or (intern-soft (symbol-name it) new)
           (define-abbrev new
             (symbol-name it)
             (symbol-value it)
             (symbol-function it)
             nil
             t)))
     old)))

(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist
                         '(minitest "\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2))
            (add-to-list 'compilation-error-regexp-alist 'minitest)))

(condition-case nil
    (progn
      (require 'ansi-color)
      (defun minitest-colorize-compilation-buffer ()
        (toggle-read-only)
        (ansi-color-apply-on-region (point-min) (point-max))
        (toggle-read-only))
      (add-hook 'compilation-filter-hook 'minitest-colorize-compilation-buffer))
  (error nil))

(provide 'minitest-mode)
;;; minitest-mode.el ends here