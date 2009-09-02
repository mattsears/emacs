(load "~/.emacs.d/mattsears/modes/css")
(load "~/.emacs.d/mattsears/modes/diff")
(load "~/.emacs.d/mattsears/modes/dired")
(load "~/.emacs.d/mattsears/modes/erlang")
(load "~/.emacs.d/mattsears/modes/javascript")
(load "~/.emacs.d/mattsears/modes/ruby")
(load "~/.emacs.d/mattsears/modes/ido")
(load "~/.emacs.d/mattsears/modes/clojure")
(load "~/.emacs.d/mattsears/modes/markdown")
(load "~/.emacs.d/mattsears/modes/buffer")
(load "~/.emacs.d/mattsears/modes/org")
(load "~/.emacs.d/mattsears/modes/erc")
(load "~/.emacs.d/mattsears/modes/snippets")

;; Wrap words in text-mode
(autoload 'longlines-mode "longlines.el" "Minor mode for editing long lines." t)
(add-hook 'text-mode-hook 'longlines-mode)

;; Other text mode improvements
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (auto-fill-mode 1)
             ))

;; Bash
(setq auto-mode-alist (cons '("\\.bash_profile" . sh-mode) auto-mode-alist))

;; Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key (kbd "C-c c") 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c k") 'mode-compile-kill)
;; Set custom flags when running the ruby command in mode-compile
(setq ruby-dbg-flags nil)

;; Indicate syntax errors
(add-to-list 'load-path "~/.emacs.d/vendor/flymake.el")
(require 'flymake)

;; Yaml
(add-to-list 'load-path "~/.emacs.d/vendor/yaml")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Git goodness
(require 'magit)
(autoload 'magit-status "magit" nil t)

;; Defunkt's Textmate
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; Midnight mode to clean up old buffers
(require 'midnight)

;; Spell checker
(add-to-list 'load-path "~/.emacs.d/vendor/flyspell.el")
(require 'flyspell)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))


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

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

;; Haml and Sass
(require 'sass-mode)
(require 'haml-mode)

;; scroll smoothly
(require 'smooth-scrolling)

;; Sudo saving
(require 'sudo)

;; Navigation bar
(require 'nav)

;; Enhanced M-x
(setq smex-save-file "~/.emacs.d/smex.save")
(require 'smex)
(smex-initialize)

;; Quicksilver like functionality
(require 'anything)
(require 'anything-config)

;; Make anything work with find-file-in-project
(defvar anything-c-source-project-files
  '((name . "Files from Current Project")
    (candidates . project-files)
    (volatile)
    (type . file)))

(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-project-files
            anything-c-source-file-name-history
            ;;anything-c-source-locateg anyth
            ;;anything-c-source-mac-spotlight
            anything-c-source-recentf
            anything-c-source-google-suggest
            ;;anything-c-source-file-caches
            ))

(setq fit-frame-inhibit-fitting-flag t)
(setq anything-selection-face 'twilight-highlight)
(setq anything-header-face 'font-lock-builtin-face)

(provide 'modes)
