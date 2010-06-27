;;----------------------------------------------------------------------------
;; Custom functions
;;----------------------------------------------------------------------------

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))

(defun insert-blank-line-after-current ()
  "Textmate style command+space"
  (interactive)
  (next-line)
  (beginning-of-line)
  (insert "\n"))

(defun open-line-below ()
  "Open a line below the line the point is at.
   Then move to that line and indent accordning to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above ()
  "Open a line above the line the point is at.
  Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun backward-delete-word ()
  "Delete work backwards without saving it to the kill ring."
  (interactive)
  (delete-region (point) (progn (backward-word) (point))))

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

(defun indent-buffer ()
  "Indents whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-buffer-or-region ()
  "Indents region if any. Otherwise whole buffer."
  (interactive)
  (if mark-active
      (call-interactively 'indent-region)
    (indent-buffer)))

(defun back-to-indentation-or-beginning-of-line ()
  "Moves point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun my-new-frame-with-new-scratch ()
  "Create a new scratch area"
  (interactive)
  (let ((one-buffer-one-frame t))
    (new-frame-with-new-scratch)))
(define-key osx-key-mode-map (kbd "A-n") 'my-new-frame-with-new-scratch)

(defun insert-soft-tab ()
  " Tab with spaces"
  (interactive)
  (insert "   "))

(defun vendor (library)
  "For loading libraries in from the vendor directory"
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (matt (concat "~/.emacs.d/mattsears/" file)))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (require library)))
    (when (file-exists-p (concat matt ".el"))
      (load matt))))

(defun shebang-to-mode ()
  "Recognize file types with shebang declaration"
  (interactive)
  (let*
      ((bang (buffer-substring (point-min) (prog2 (end-of-line) (point) (move-beginning-of-line 1))))
       (mode (progn
               (string-match "^#!.+[ /]\\(\\w+\\)$" bang)
               (match-string 1 bang)))
       (mode-fn (intern (concat mode "-mode"))))
    (when (functionp mode-fn)
      (funcall mode-fn))))
(add-hook 'find-file-hook 'shebang-to-mode)

(defun duplicate-line ()
  "Duplicate the current line"
  (interactive)
  (beginning-of-line)
  (copy-region-as-kill (point) (progn (end-of-line) (point)))
  (matts-next-line)
  (yank)
  (beginning-of-line)
  (indent-according-to-mode))

(defun matts-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun copy-line()
  "Copy the current line"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (column (current-column)))
    (copy-region-as-kill beg end)))

(defun reset-window-position ()
  "Reset windows and frames"
  (interactive)
  (delete-other-windows)
  (set-frame-position (selected-frame) 325 60)
  )

(one-buffer-one-frame-mode 0)
(defun my-close-current-window-asktosave ()
  "Kill the buffer, but not the window"
  (interactive)
  (kill-buffer (current-buffer)))

(one-buffer-one-frame-mode 0)
(defun matts-close-and-delete-window ()
  "Kill the current frame and the window"
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (delete-window))

(defun matts-split-window-three-ways ()
  "Reset windows and frames with room"
  (interactive)
  (delete-other-windows)
  (set-frame-position (selected-frame) 50 70)
  (split-window-horizontally)
  ;;(other-window 1)
  (split-window-vertically)
  (other-window 1)
  )
(defun matts-delete-whole-line ()
  "Deletes the whole line with copying the text to the kill-ring"
  (interactive)
  (beginning-of-line)
  (setq matts-begin-point (point))
  (forward-line 1)
  (setq matts-end-point (point))
  (delete-region matts-begin-point matts-end-point))

;; From http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun matts-flip-windows ()
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

(defun matts-popup-commands ()
  "Show a popup menu of commands."
  (interactive)
  (eval-expression (car (read-from-string (choose-from-menu "My Menu"
                                                            (list
															 (cons "IRC" "(call-interactively 'erc)")
         													 (cons "Shell" "(eshell)")
															 (cons "Bookmarks" "(call-interactively 'list-bookmarks)")
															 (cons "Export Calendar" "(call-interactively 'matts-org-export-icalendar)")
                                                             (cons "-" "")
 															 (cons "Today's Agenda" "(org-agenda-list)")
															 (cons "Todos" "(matts-todos)")
                               (cons "Appointments" "(matts-appointments)")
														     (cons "-" "")
															 (cons "Show/hide line numbers " "(linum)")
                                                             (cons "Nuke all buffers " "(nuke-all-buffers)")
                                                             (cons "Calendar" "(calendar)")
                                                             (cons "Reset Window" "(reset-window-position)")
                                                             (cons "Split Window" "(matts-split-window-three-ways)")
                                                             (cons "Reload Emacs" "(load-file \"~/.emacs\")")
                                                             ))))) )

(defun matts-popup-symbols ()
  "Popups for the current buffer's symbols"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
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
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))

    (let* ((selected-symbol (choose-from-menu "Symbols " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position)))
  )

(defun passenger-restart (&optional starting)
  "Restart apache - requred project-root variable"
  (interactive)
  (when (null project-root)
    (error "Can't find any project"))
  (shell-command-to-string
   (concat
    "touch "
    project-root
    "/tmp/restart.txt")) "\n" t)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; keybindings
(global-set-key [pause] 'toggle-window-dedicated)

(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;; Automatically indent region when code is pasted
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode lisp-mode ruby-mode objc-mode nxml-mode
                                           javascript-mode latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))


(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
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
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; Borrowed from defunkt's textmate.el http://github.com/defunkt/textmate.el/blob/master/textmate.el
(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))

(provide 'defuns)
