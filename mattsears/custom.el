; Set custom variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-autoface-mode nil)
 '(aquamacs-customization-version-id 172 t)
 '(aquamacs-save-options-on-quit nil)
 '(aquamacs-scratch-file nil)
 '(column-number-mode nil)
 '(default-frame-alist (quote ((tool-bar-lines . 0) (fringe) (right-fringe) (left-fringe . 1) (vertical-scroll-bars) (cursor-type . box) (menu-bar-lines . 0) (background-color . "#0C1021") (background-mode . dark) (border-color . "#dedede") (cursor-color . "#AFAFAF") (foreground-color . "#F8F8F8") (mouse-color . "sienna1"))))
 '(global-hl-line-mode t)
 '(load-home-init-file t t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-custom-commands (quote (("d" todo "DELEGATED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "<[^>
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/org/home.org" "~/org/work.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-combined-agenda-icalendar-file "~/org/agenda.ics" t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/org/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote ((116 "* TODO %?
  %u" "~/todo.org" "Tasks") (110 "* %u %?" "~/notes.org" "Notes"))))
 '(org-reverse-note-order t t)
 '(org-tag-alist (quote (("URGENT" . 117) ("@CALL" . 99) ("@ERRANDS" . 101))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(tabbar-mode nil nil (tabbar))
 '(transient-mark-mode t)
 '(x-stretch-cursor t))

(setq transparency-level 90)
;; (set-frame-parameter nil 'alpha transparency-level)
;; (add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))

;;  The only I can get a bar cursor on mac os
(setq initial-frame-alist
      (cons '(cursor-type . bar)(copy-alist initial-frame-alist)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit prog-mode-default :slant normal :weight normal :height 120 :family "monaco"))))
 '(text-mode-default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))) t))
