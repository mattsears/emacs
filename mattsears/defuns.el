;; TODO: Run a remote Rails console
(defun rails-remote-console()
  (interactive)
  (run-ruby "ssh server /var/www/apps/appname/current/script/console"))

;; For indenting an entire file
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Textmate style command+space
(defun insert-blank-line-after-current ()
  (interactive)
  (next-line)
  (beginning-of-line)
  (insert "\n"))
(define-key global-map [M-return] 'insert-blank-line-after-current)

;; Create a new scratch area
(defun my-new-frame-with-new-scratch ()
  (interactive)
  (let ((one-buffer-one-frame t))
    (new-frame-with-new-scratch)))
(define-key osx-key-mode-map (kbd "A-n") 'my-new-frame-with-new-scratch)

;; Tab with spaces
(defun insert-soft-tab ()
  (interactive)
  (insert "   "))

;; Kill the current frame, but not window
(one-buffer-one-frame-mode 0)
(defun my-close-current-window-asktosave ()
  (interactive)
  (let ((one-buffer-one-frame t))
    (kill-buffer (current-buffer))))
(define-key osx-key-mode-map (kbd "A-w") 'my-close-current-window-asktosave)

;; For loading libraries in from the vendor directory
(defun vendor (library)
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

;; Recognize file types with shebang declaration
(defun shebang-to-mode ()
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

;; Duplicate the current line
(defun duplicate-line () 
  (interactive)
    (beginning-of-line)
    (copy-region-as-kill (point) (progn (end-of-line) (point)))
    (textmate-next-line)
    (yank)
    (beginning-of-line)
    (indent-according-to-mode))

;; Reset windows and frames
(defun reset-window-position ()
  (interactive)
    (nuke-some-buffers)
    (maximize-frame)
    (set-frame-position (selected-frame) 50 70)
	(split-window-horizontally)
	(other-window 0) 
)

;; Kills live buffers, leaves some emacs work buffers
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-some-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
   LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (not (string-equal name "*Messages*"))
	  ;; (not (string-equal name "*Buffer List*"))
	   (not (string-equal name "*buffer-selection*"))
	   (not (string-equal name "*Shell Command Output*"))
	   (not (string-equal name "*scratch*"))
	   (/= (aref name 0) ? )
	   (if (buffer-modified-p buffer)
	       (if (yes-or-no-p
		    (format "Buffer %s has been edited. Kill? " name))
		   (kill-buffer buffer))
	     (kill-buffer buffer))))
    (setq list (cdr list))))


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

;; Handy text filler
(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;; Scroll without moving cursor
(defun scroll-down-keep-cursor () 
;; the text one line down while keeping the cursor 
    (interactive) 
    (scroll-down 1))
(defun scroll-up-keep-cursor () 
    ;; Scroll the text one line up while keeping the cursor 
    (interactive) 
    (scroll-up 1))
(global-set-key '[C-M-up] 'scroll-down-keep-cursor)
(global-set-key '[C-M-down] 'scroll-up-keep-cursor)

;; From Steve Yegge: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

;; Clears the screen in eshell mode
(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(provide 'defuns)
