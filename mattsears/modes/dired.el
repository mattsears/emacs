;;----------------------------------------------------------------------------
;; Dired, the directory editor
;;----------------------------------------------------------------------------
(require 'dired)
(require 'wdired)
(require 'dired-x)

;; Hide certain files/directories from showing up in dired mode
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq "coverage" eol)          ;; code coverage files
              )))

(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)

(setq dired-details-hidden-string "[ ... ] ")
(setq dired-listing-switches "-l")
(setq directory-free-space-args "-kh")
(setq dired-omit-size-limit nil)
(setq dired-dwim-target t)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies (quote always))

(add-hook 'dired-mode-hook
          '(lambda()
             (define-key dired-mode-map [delete] 'dired-do-delete)
             (define-key dired-mode-map [backspace] 'matts-dired-up-directory)
             (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
             (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
             (define-key dired-mode-map "n" 'dired-touch-now)
             (local-set-key "\C-m" 'matts-dired-find-file)
             (local-set-key "^" 'matts-dired-up-directory)
             (define-key dired-mode-map [return] 'matts-dired-find-file)))

;;----------------------------------------------------------------------------
;; Dired related functions
;;----------------------------------------------------------------------------

;; Remap 'o' in dired mode to open a file
(defun matts-dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))
(define-key dired-mode-map "o" 'matts-dired-open-mac)

(put 'dired-find-alternate-file 'disabled nil)
(defun matts-dired-up-directory ()
  "Go up one directory and don't create a new dired buffer but
reuse the current one."
  (interactive)
  (find-alternate-file ".."))

(defun dired-touch-now (touch-file)
  "Do `touch' command with TOUCH-FILE."
  (interactive "sNew file: ")
  (cd (dired-current-directory))
  (shell-command
   (concat "touch \""
           ;; if filename is begin with `-', add '-- ' before file-name
           (if (string-match "^-.*" touch-file) "-- ")
           touch-file "\""))
  (sit-for 0.1)
  (revert-buffer)
  (dired-goto-file (concat (dired-current-directory) touch-file)))

(defun matts-dired-find-file ()
  "Open the file or directory without opening a new buffer"
  (interactive)
  (let ((filename (dired-get-filename))
        (orig (current-buffer)))
    (if (file-directory-p filename)
        (find-alternate-file filename)
      (dired-find-file)
      (kill-buffer orig))))

