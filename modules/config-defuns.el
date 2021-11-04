
;;----------------------------------------------------------------------------
;; Custom functions
;;----------------------------------------------------------------------------

(defun matts/arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

(defun matts/shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun matts/shift-right (count)
  (interactive "p")
  (matts/shift-text count))

(defun matts/shift-left (count)
  (interactive "p")
  (matts/shift-text (- count)))

(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun matts/toggle-quotes ()
  (interactive)
  (let* ((beg (nth 8 (syntax-ppss)))
         (orig-quote (char-after beg))
         (new-quote (pcase orig-quote
                      (?\' ?\")
                      (?\" ?\'))))
    (save-restriction
     (widen)
     (save-excursion
      (catch 'done
        (unless new-quote
          (message "Not inside a string")
          (throw 'done nil))
        (goto-char beg)
        (delete-char 1)
        (insert-char new-quote)
        (while t
          (cond ((eobp)
                 (throw 'done nil))
                ((= (char-after) orig-quote)
                 (delete-char 1)
                 (insert-char new-quote)
                 (throw 'done nil))
                ((= (char-after) ?\\)
                 (forward-char 1)
                 (when (= (char-after) orig-quote)
                   (delete-char -1))
                 (forward-char 1))
                ((= (char-after) new-quote)
                 (insert-char ?\\)
                 (forward-char 1))
                (t (forward-char 1)))))))))

;;----------------------------------------------------------------------------
;; Evil mode helpers
;;----------------------------------------------------------------------------

(defun my-evil-terminal-cursor-change ()
  (interactive)
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

;;To get the cursor to change in insert mode in iTerm
(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond ((evil-insert-state-p) '("#8700ff" . "#ffffff"))
                     ((evil-visual-state-p) '("#646c9c" . "#ffffff"))
                     ((evil-normal-state-p) default-color)
                     (t '("#1d1f21" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

                                        ; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my-save-if-bufferfilename ()
  (interactive)
  (if (buffer-file-name)
      (progn
        (evil-escape)
        (save-buffer))
    (message "no file is associated to this buffer: do nothing")
    ))

;;----------------------------------------------------------------------------
;; Buffer Utils
;;----------------------------------------------------------------------------

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun untabify-buffer ()
  "Replaces all tabs in the buffer with spaces."
  (interactive)
  (untabify (point-min) (point-max)))

(defun untabify-buffer-or-region ()
  "Replaces all tabs in the buffer with spaces."
  (interactive)
  (if mark-active
      (untabify-buffer)
    (untabify (point-min) (point-max))))

(defun matts/nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'cleanup-buffer)

(defun insert-soft-tab ()
  " Tab with spaces"
  (interactive)
  (insert "   "))

(defun switch-to-other-buffer ()
  "Toggle previous buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  ;; Don't switch back to the ibuffer!!!
  (if (buffer-exists "*Buffers*")  (kill-buffer "*Ibuffer*"))
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

(defun matts/hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun matts/align-columns (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))


;;----------------------------------------------------------------------------
;; Window helpers
;;----------------------------------------------------------------------------

(defun my-close-current-window-asktosave ()
  "Kill the buffer, but not the window"
  (interactive)
  (kill-buffer (current-buffer)))

(defun matts-close-and-delete-window ()
  "Kill the current frame and the window and go back to the first window"
  (interactive)

  (matts/first-window)
  (other-window 1)
  (kill-buffer (current-buffer))
  (delete-window))

(defun matts/last-window ()
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun matts/first-window ()
  (interactive)
  (select-window (frame-first-window)))

(defun matts/flip-windows ()
  ";; Swap windows if in split-screen mode"
  (interactive)
  (let ((cur-buffer (current-buffer))
        (top-buffer)
        (bottom-buffer))
    (pop-to-buffer (window-buffer (frame-first-window)))
    (setq top-buffer (current-buffer))
    (other-window 1)
    (setq bottom-buffer (current-buffer))
    (switch-to-buffer top-buffer)
    (other-window -1)
    (switch-to-buffer bottom-buffer)
    (pop-to-buffer cur-buffer)))


;;----------------------------------------------------------------------------
;; IDO related functions
;;----------------------------------------------------------------------------

(defun matts-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recent open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(defun choose-from-menu (menu-title menu-items)
  "Choose from a list of choices from a popup menu."
  (let ((item)
        (item-list))
    (while menu-items
      (setq item (car menu-items))
      (if (consp item)
          (setq item-list (cons (cons (car item) (cdr item) ) item-list))
        (setq item-list (cons (cons item item) item-list)))
      (setq menu-items (cdr menu-items)))
    (x-popup-menu
     `((500 200) ,(selected-frame))
     (list menu-title (cons menu-title (nreverse item-list))))))

(require 'noflet)
(defun matts-find-symbol ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (noflet ((addsymbols
            (symbol-list)
            (when (listp symbol-list)
              (dolist (symbol symbol-list)
                (let ((name nil) (position nil))
                  (cond
                   ((and (listp symbol) (imenu--subalist-p symbol))
                    (addsymbols symbol))

                   ((listp symbol)
                    (setq name (car symbol))
                    (setq position (cdr symbol)))

                   ((stringp symbol)
                    (setq name symbol)
                    (setq position
                          (get-text-property 1 'org-imenu-marker symbol))))

                  (unless (or (null position) (null name))
                    (add-to-list 'symbol-names name)
                    (add-to-list 'name-and-pos (cons name position))))))))
          (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning
    ;; of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols
                (delq nil (mapcar
                           (lambda (symbol)
                             (if (string-match regexp symbol) symbol))
                           symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc
             (lambda (symbol)
               (setq symbol-names (cons symbol (delete symbol symbol-names))))
             matching-symbols)))))
    (let* ((selected-symbol (ivy-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (push-mark)
      (if (overlayp position)
          (goto-char (overlay-start position))
        (goto-char position)))))

;;----------------------------------------------------------------------------
;; File related helpers
;;----------------------------------------------------------------------------

(defun matts/delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

(defun matts/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun markdown-preview-file ()
  "This function will open Marked.app and monitor the current markdown document
for anything changes.  In other words, it will live reload and convert the
markdown documment"
  (interactive)
  (shell-command
   (format "open -a /Applications/Setapp/Marked.app %s"
           (shell-quote-argument (buffer-file-name))))
  )

;;----------------------------------------------------------------------------
;; UI-related helpers
;;----------------------------------------------------------------------------

(defun matts/reload-color-theme ()
  "Reloads the color themes. Handy when experimenting with various colors"
  (interactive)
  (load-file "/Users/matt/emacs/modules/config-theme.el")
  )

;;----------------------------------------------------------------------------
;; Company mode related functions
;;----------------------------------------------------------------------------

(defun company-pseudo-tooltip-on-explicit-action (command)
  "`company-pseudo-tooltip-frontend', but only on an explicit action."
  (when (company-explicit-action-p)
    (setq tip-showing t)
    (company-pseudo-tooltip-frontend command)))

(defun company-echo-metadata-on-explicit-action-frontend (command)
  "`company-mode' front-end showing the documentation in the echo area."
  (pcase command
    (`post-command (when (company-explicit-action-p)
                     (company-echo-show-when-idle 'company-fetch-metadata)))
    (`hide
     (company-echo-hide)
     (setq tip-showing nil)
     )))


;;----------------------------------------------------------------------------
;; Dired related functions
;;----------------------------------------------------------------------------

(defun matts-dired-open-mac ()
  "Remap 'o' in dired mode to open a file"
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

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


(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;;; Pick one: projectile or find-file-in-project ; (projectile-project-root)
           (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(defun matts/font-lock-custom-keywords ()
  "Highlight a bunch of well known comment annotations. This functions
should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'matts/font-lock-custom-keywords)


(defun matts/open-env ()
  "Open the projects .env file"
  (interactive)
  (when-let ((root (projectile-project-root)))
    (find-file-other-window (expand-file-name ".env" root)))
  )

(defun matts/restart-rails-server ()
  "Restarts the running Rails server"
  (interactive)
  (shell-command "pkill -USR1 puma-dev")
  )

(defun matts/format-html ()
  "run a command on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "bundle exec htmlbeautifier %s" (shell-quote-argument (buffer-file-name))  ))
  (revert-buffer t t t))

(defun matts/change-number-at-point (change increment)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number increment)))
        (goto-char point)))))

(defun matts/increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (matts/change-number-at-point '+ (or increment 1)))

(defun matts/decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (matts/change-number-at-point '- (or increment 1)))

(defun matts/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(provide 'config-defuns)
