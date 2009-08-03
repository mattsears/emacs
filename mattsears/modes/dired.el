;; Dired
(require 'dired)
(require 'wdired)

(setq dired-details-hidden-string "[ ... ] ")
(setq dired-listing-switches "-l")
(setq directory-free-space-args "-kh")
(setq dired-omit-size-limit nil)
(setq dired-dwim-target t)
(setq dired-recursive-deletes 'top)

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

(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you double-click on (EVENT)."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

(defun matts-dired-find-file ()
  "Open the file or directory without opening a new buffer"
  (interactive)
  (let ((filename (dired-get-filename))
        (orig (current-buffer)))
    (if (file-directory-p filename)
        (find-alternate-file filename)
      (dired-find-file)
      (kill-buffer orig))))

(add-hook 'dired-mode-hook
          '(lambda()
             (define-key dired-mode-map [delete] 'dired-do-delete)
             (define-key dired-mode-map [backspace] 'matts-dired-up-directory)
             (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
             (define-key dired-mode-map [C-down-mouse-1] 'mouse-buffer-menu)
             (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
             (local-set-key "\C-m" 'matts-dired-find-file)
             (define-key dired-mode-map [double-down-mouse-1] 'dired-mouse-find-file)
             (local-set-key "^" 'matts-dired-up-directory)
             (define-key dired-mode-map [return] 'matts-dired-find-file)))

