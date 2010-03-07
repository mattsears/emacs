;;----------------------------------------------------------------------------
;; Anything - Quicksilver like switcher
;;----------------------------------------------------------------------------

;; Make anything work with find-file-in-project
(defvar anything-c-source-project-files
  '((name . "Files from Current Project")
    (candidates . project-files)
    (volatile)
    (type . file)))

;; See: http://www.emacswiki.org/emacs/AnythingSources#toc62

(defvar anything-c-source-git-project-files-cache nil "(path signature cached-buffer)")
    (defvar anything-c-source-git-project-files '((name . "Files from Current GIT Project")
	(init . (lambda ()

	  		(let* ((anything-git-top-dir (file-truename (magit-get-top-dir (if (buffer-file-name)
									(file-name-directory (buffer-file-name))
                  default-directory))))

			(default-directory anything-git-top-dir)


			 (signature (magit-shell (magit-format-git-command "rev-parse --verify HEAD" nil))))

		    (unless (and anything-c-source-git-project-files-cache
				 (third anything-c-source-git-project-files-cache)
				 (equal (first anything-c-source-git-project-files-cache) anything-git-top-dir)
				 (equal (second anything-c-source-git-project-files-cache) signature))
		      (if (third anything-c-source-git-project-files-cache)
			  (kill-buffer (third anything-c-source-git-project-files-cache)))
		      (setq anything-c-source-git-project-files-cache
			    (list anything-git-top-dir
				  signature
				  (anything-candidate-buffer 'global)))
		      (with-current-buffer (third anything-c-source-git-project-files-cache)
			(dolist (filename (mapcar (lambda (file) (concat default-directory file))
						  (magit-shell-lines (magit-format-git-command "ls-files" nil))))
			  (insert filename)
			  (newline))))
		    (anything-candidate-buffer (third anything-c-source-git-project-files-cache)))))

        (type . file)
	(candidates-in-buffer)))

(setq anything-sources
      (list anything-c-source-git-project-files
         	anything-c-source-git-project-files-cache
            anything-c-source-bookmarks
            ;; anything-c-source-project-files
            ;;anything-c-source-file-name-history
            ;;anything-c-source-recentf
            ))

(setq fit-frame-inhibit-fitting-flag t)
(setq anything-selection-face 'lazy-highlight-face)
(setq anything-header-face 'font-lock-builtin-face)

(provide 'anything)
