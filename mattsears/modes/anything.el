;;----------------------------------------------------------------------------
;; Anything - Quicksilver like switcher
;;----------------------------------------------------------------------------

;; Make anything work with find-file-in-project
(defvar anything-c-source-project-files
  '((name . "Files from Current Project")
    (candidates . project-files)
    (volatile)
    (type . file)))

(defvar anything-c-source-cheat
  '((name . "Cheat Sheets")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (with-current-buffer (anything-candidate-buffer 'global)
                  (call-process-shell-command
                   "cheat sheets" nil  (current-buffer))
                  (goto-char (point-min))
                  (forward-line 1)
                  (delete-region (point-min) (point))
                  (indent-region (point) (point-max) -2)))))
    (candidates-in-buffer)
    (action . (lambda (entry)
                (let ((buf (format "*cheat sheet:%s*" entry)))
                  (unless (get-buffer buf)
                    (call-process "cheat" nil (get-buffer-create buf) t entry))
                  (display-buffer buf)
                  (set-window-start (get-buffer-window buf) 1))))))

(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-project-files
            anything-c-source-file-name-history
            anything-c-source-recentf
            anything-c-source-cheat
            ))

(setq fit-frame-inhibit-fitting-flag t)
(setq anything-selection-face 'twilight-highlight)
(setq anything-header-face 'font-lock-builtin-face)

(provide 'anything)
