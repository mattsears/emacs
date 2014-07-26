;;----------------------------------------------------------------------------
;; Custom functions
;;----------------------------------------------------------------------------

(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;;----------------------------------------------------------------------------
;; Helpers for moving text around
;;----------------------------------------------------------------------------

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun text-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun text-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (text-shift-right (* -1 (or arg 1))))

(defun copy-line()
  "Copy the current line"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (column (current-column)))
    (copy-region-as-kill beg end)))

(defun matts-delete-whole-line ()
  "Deletes the whole line with copying the text to the kill-ring"
  (interactive)
  (beginning-of-line)
  (setq matts-begin-point (point))
  (forward-line 1)
  (setq matts-end-point (point))
  (delete-region matts-begin-point matts-end-point))

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
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

(defun nuke-all-buffers ()
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

(defun back-to-indentation-or-beginning-of-line ()
  "Moves point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

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
  (if (buffer-exists "*Ibuffer*")  (kill-buffer "*Ibuffer*"))
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun matts-ibuffer ()
  "Open ibuffer with cursour pointed to most recent buffer name"
  (interactive)
  (let ((recent-buffer-name (buffer-name)))
    (ibuffer)
    (ibuffer-jump-to-buffer recent-buffer-name)))

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
  (matts-first-window)
  (other-window 1)
  (kill-buffer (current-buffer))
  (delete-window))

(defun matts-first-window ()
  (interactive)
  (select-window (frame-first-window)))

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

(defun switch-to-scratch ()
  "switch to scratch, grab the region if it's active"
  (interactive)
  (let ((contents
         (and (region-active-p)
              (buffer-substring (region-beginning)
                                (region-end)))))
    (switch-to-buffer "*scratch*")
    (if contents
        (progn
          (goto-char (buffer-end 1))
          (insert contents)))))

;;----------------------------------------------------------------------------
;; IDO related functions
;;----------------------------------------------------------------------------

(defun matts-ido-find-project ()
  (interactive)
  (let ((project-root (concat "~/Workspace/"  (ido-completing-read "Project: "
                                                                   (directory-files "~/Workspace/" nil "^[^.]")))))
    ;; (nuke-all-buffers)
    (projectile-switch-project-by-name project-root)))


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

(defun matts-ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (matts-ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (matts-ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

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

;;----------------------------------------------------------------------------
;; File related helpers
;;----------------------------------------------------------------------------

(defun delete-this-buffer-and-file ()
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

(defun rename-file-and-buffer (new-name)
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

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun find-file-as-root ()
  "Find a file as root."
  (interactive)
  (let* ((parsed (when (tramp-tramp-file-p default-directory)
                   (coerce (tramp-dissect-file-name default-directory)
                           'list)))
         (default-directory
           (if parsed
               (apply 'tramp-make-tramp-file-name
                      (append '("sudo" "root") (cddr parsed)))
             (tramp-make-tramp-file-name "sudo" "root" "localhost"
                                         default-directory))))
    (call-interactively 'find-file)))

(defun toggle-alternate-file-as-root (&optional filename)
  "Toggle between the current file as the default user and as root."
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (parsed (when (tramp-tramp-file-p filename)
                   (coerce (tramp-dissect-file-name filename)
                           'list))))
    (unless filename
      (error "No file in this buffer."))
    (find-alternate-file
     (if (equal '("sudo" "root") (butlast parsed 2))
         ;; As non-root
         (if (or
              (string= "localhost" (nth 2 parsed))
              (string= (system-name) (nth 2 parsed)))
             (nth -1 parsed)
           (apply 'tramp-make-tramp-file-name
                  (append (list tramp-default-method nil) (cddr parsed))))
       ;; As root
       (if parsed
           (apply 'tramp-make-tramp-file-name
                  (append '("sudo" "root") (cddr parsed)))
         (tramp-make-tramp-file-name "sudo" "root" "localhost" filename))))))

(defun markdown-preview-file ()
  "This function will open Marked.app and monitor the current markdown document
for anything changes.  In other words, it will live reload and convert the
markdown documment"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name))))
  )

;;----------------------------------------------------------------------------
;; UI-related helpers
;;----------------------------------------------------------------------------

(require 'powerline)
(defun powerline-clean-theme ()
  "Customize the modeline with help from the powerline package"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (lhs (list (powerline-raw (concat "L%l") nil 'l)))
                          (rhs (list
                                (if (buffer-modified-p)
                                    (powerline-raw "*" nil 'r))
                                (powerline-raw (concat "")  nil 'r)
                                (powerline-raw (concat "(%m) %p")  nil 'l)))
                          (center (list (powerline-raw "%b" nil) )))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center nil (/ (powerline-width center) 2.0))
                      (powerline-render center)
                      (powerline-fill nil (powerline-width rhs))
                      (powerline-render rhs)))))))
(powerline-clean-theme)

(defun reload-color-theme ()
  "Reloads the color themes. Handy when experimenting with various colors"
  (interactive)
  (load-file "~/.emacs.d/mattsears/color-theme-neptune.el")
  (color-theme-neptune))

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region."
  (interactive "r")
  (save-excursion
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (incf end))
    (indent-region begin end nil)))

(defun beautify-js ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "js-beautify -q -w 80 --no-preserve-newlines -f -" (current-buffer) t)))

;;----------------------------------------------------------------------------
;; Ruby related functions
;;----------------------------------------------------------------------------

(defun my-sass-comment-fix ()
  "Change the default commenting sequence for sass"
  (set 'comment-start "//")
  )

(defun rails-console ()
  "Create a rails console process, if one doesn't exist. And
switch to *rails-console* buffer."
  (interactive)
  (if (null (get-buffer "*rails-console*"))
      (progn
        (multi-term)
        (term "/bin/bash")
        (term-send-string (get-buffer-process "*terminal*") "rails console\n")
        (switch-to-buffer "*terminal*")
        (rename-buffer "rails-console")
        (term-line-mode))
    (switch-to-buffer "rails-console")))

(defun is-rails-project ()
  (when (project-root)
    (file-exists-p (expand-file-name "config/environment.rb" (project-root)))))

(defun run-rails-test-or-ruby-buffer ()
  (interactive)
  (if (is-rails-project)
      (let* ((path (buffer-file-name))
             (filename (file-name-nondirectory path))
             (test-path (expand-file-name "test" (project-root)))
             (command (list ruby-compilation-executable "-I" test-path path)))
        (pop-to-buffer (ruby-compilation-do filename command)))
    (ruby-compilation-this-buffer)))

(define-key global-map (kbd "s-r") 'run-rails-test-or-ruby-buffer)

(defun operate-on-point-or-region (fn)
  "Get the current unspaced string at point, or the current
region, if selected, and replace it with the return value of fn -
an ordinary defun."
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (funcall fn (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)))

;; Change a string to a ruby symbol, note: naive operation
(defun ruby-make-symbol-at-point ()
  "Dirt simple, just prefix current word with a colon"
  (interactive)
  (operate-on-point-or-region 'ruby-prepend-colon))

(defun ruby-prepend-colon (s) ""
  (format ":%s" s))

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c :") 'ruby-make-symbol-at-point))

(defun erb-to-haml ()
  "run html2haml on current buffer"
  (interactive)
  (save-excursion
    (save-buffer)
    (shell-command (concat "html2haml --erb " (buffer-file-name) " 2> /dev/null")
                   (current-buffer)
                   )))

(defun erb-region-to-haml ()
  "Convert selected region to haml"
  (interactive)

  (let ((deactivate-mark nil)

        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))

    (shell-command-on-region beg end "html2haml --erb -s" (buffer-name) t)
    ))

(defun erb-to-haml-and-change-file-extension ()
  "run html2haml on current buffer"
  (interactive)
  (save-excursion
    (save-buffer)
    (shell-command (concat "html2haml --erb " (buffer-file-name) " 2> /dev/null")
                   (current-buffer)
                   )
    (let ((new-name (replace-regexp-in-string "erb" "haml" (buffer-file-name))))
      (rename-file (buffer-file-name) new-name)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)
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

(defun matts-dired-find-file ()
  "Open the file or directory without opening a new buffer"
  (interactive)
  (let ((filename (dired-get-filename))
        (orig (current-buffer)))
    (if (file-directory-p filename)
        (find-alternate-file filename)
      (dired-find-file)
      (kill-buffer orig))))


;;----------------------------------------------------------------------------
;; Tags
;;----------------------------------------------------------------------------

(defvar ctags-options "" "Options for tags generation")
(setq tags-revert-without-query 1)

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun build-ctags()
  (interactive)
  (let ((root (project-root)))
    (let ((my-tags-file (concat root "TAGS")))
      (message "Regenerating TAGS file: %s" my-tags-file)
      (if (file-exists-p my-tags-file)
          (delete-file my-tags-file))
      (shell-command
       (format "ctags -e -R --exclude=db --exclude=.git --exclude=tmp --exclude=test --exclude=.#* %s -f %s %s"
               ctags-options my-tags-file root))
      (if (get-file-buffer my-tags-file)
          (kill-buffer (get-file-buffer my-tags-file)))
      (visit-tags-table my-tags-file))))

;;----------------------------------------------------------------------------
;; Project Roots
;;----------------------------------------------------------------------------

(defvar *project-roots*
  '(".git" ".hg" "Rakefile" "Makefile" "README" "build.xml" ".emacs-project" "Gemfile")
  "The presence of any file/directory in this list indicates a project root.")
(defvar *project-root* nil
  "Used internally to cache the project root.")

(defun root-match(root names)
  (member (car names) (directory-files root)))

(defun root-matches(root names)
  (if (root-match root names)
      (root-match root names)
    (if (eq (length (cdr names)) 0)
'nil
      (root-matches root (cdr names))
      )))

(defun project-root ()
  "Returns the current project root."
  (when (or
         (null *project-root*)
         (not (string-match *project-root* default-directory)))
    (let ((root (find-project-root)))
      (if root
          (setq *project-root* (expand-file-name (concat root "/")))
        (setq *project-root* nil))))
  *project-root*)

(defun find-project-root (&optional root)
  "Determines the current project root by recursively searching for an indicator."
  (when (null root) (setq root default-directory))
  (cond
   ((root-matches root *project-roots*)
    (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t (find-project-root (concat (file-name-as-directory root) "..")))))

;;----------------------------------------------------------------------------
;; Personal Productivity helpers
;;----------------------------------------------------------------------------

(defun matts-quick-menu ()
  "Show a popup menu of commands."
  (interactive)
  (eval-expression
   (car
    (read-from-string
     (choose-from-menu "Quick Menu"
                       (list
                        ;;(cons "Project Explorer" "(call-interactively 'project-explorer-open)")
                        (cons "Calendar" "(call-interactively 'cfw:open-calendar-buffer)")
                        (cons "Bookmarks" "(call-interactively 'list-bookmarks)")
                        (cons "Bookmark This File" "(call-interactively 'bookmark-set)")
                        (cons "Open Notebook " "(call-interactively 'notebook)")
                        (cons "-" "")
                        (cons "Open Shell " "(call-interactively 'visit-term-buffer)")
                        (cons "Lookup HTTP code " "(call-interactively 'hc)")
                        ))))))

(defun todos ()
  "Quick open of my todos"
  (interactive)
  (let ((todo-file (concat "~/Dropbox/Notes/todos.org")))
    (find-file todo-file)
    ))

(defun goals ()
  (interactive)
  (let ((file (concat "~/Dropbox/Notes/goals.md")))
    (find-file file)
    ))

(defun projects()
  (interactive)
  (let ((file (concat "~/Dropbox/Notes/projects.md")))
    (find-file file)))

(defun business()
  (interactive)
  (let ((file (concat "~/Dropbox/Notes/business.md")))
    (find-file file)
    ))

(defun expenses()
  (interactive)
  (let ((file (concat "~/Dropbox/Notes/expenses.org")))
    (find-file file)
    ))

(defun notebook ()
  "Quick finder for my note documents that I want to be handy at all times"
  (interactive)
  (let ((note-file (concat "~/Dropbox/Notes/"
                           (ido-completing-read "Notes: "
                                                (directory-files "~/Dropbox/Notes" nil "^[^.]")))))
    (find-file note-file)
    ))

(defun matts-setup-special-key-map ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "t") 'todos)
      (define-key prefix-map (kbd "g") 'goals)
      (define-key prefix-map (kbd "b") 'business)
      (define-key prefix-map (kbd "p") 'projects)
      (define-key prefix-map (kbd "n") 'notebook)
      (define-key prefix-map (kbd "e") 'expenses)
      (define-key global-map (kbd "C-c m") prefix-map)
      map)))

(matts-setup-special-key-map)

(provide 'defuns)
