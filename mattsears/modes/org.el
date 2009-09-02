;;(require 'org-install)
(setq load-path (cons "~/.emacs.d/vendor/org-mode/lisp" load-path))

;; Use IDO for auto-complete
(setq org-completion-use-ido t)

;; Do not fold items on startup
(setq org-startup-folded "showall")

;; Use single stars instead
(setq org-hide-leading-stars t)

;; Include todos in icalendar export
(setq org-icalendar-include-todo t)

;; The location of the icalendar fiel
(setq org-combined-agenda-icalendar-file "~/org/org.ics")

;; Format the agenda grid
(setq org-agenda-time-grid '((daily require-timed)
                             "--------------------"
                             (800 1000 1200 1400 1600 1800 2000 2200)))

;; Fast todo selection allows changing from any task todo state to any other
;; state directly by selecting the appropriate key from the fast todo selection key menu
(setq org-use-fast-todo-selection t)

;; If things are done, then skip them
(setq org-agenda-skip-deadline-if-done t)

;; Keep the recent notes on top
(setq org-reverse-note-order t)
(setq org-cycle-include-plain-lists nil)

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(eval-after-load 'org
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))
     ))

;; Key binding for remember
(define-key global-map [(control meta ?r)] 'remember)

;; Remember
(vendor 'remember)
(require 'remember)
(require 'org-remember)

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

;; 3 remember templates for TODO tasks, Notes, and Phone calls

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/todo.org" "Tasks")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.org")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/journal.org" "New Ideas")))

;; (setq org-remember-templates (quote (("todo" ?t "* TODO %?
;;   %u" nil bottom nil)
;;                                      ("note" ?n "* %?                                        :NOTE:
;;   %u" nil bottom nil)
;;                                      ("phone" ?p "* PHONE %:name - %:company -                :PHONE:
;;   Contact Info: %a
;;   %u" nil bottom nil))))

;; Default states for todos
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE" "DEFERRED" "CANCELLED"))
(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                     ("STARTED" :foreground "blue" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold)
                                     ("SOMEDAY" :foreground "magenta" :weight bold)
                                     ("CANCELLED" :foreground "forest green" :weight bold)
                                     ("DEFERRED" :foreground "blue" :weight bold)
                                     )))

(setq org-agenda-custom-commands
      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
              ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("n" "Notes" tags "NOTE" nil))))

;; Refiling

;; Use IDO for target completion
(setq org-completion-use-ido t)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; Custom settings
(custom-set-variables
 '(org-agenda-files (quote ("~/org/home.org"
                            "~/org/work.org"
                            "~/org/projects/house.org"
                            "~/org/projects/marketdrums.org"
                            "~/org/projects/cms.org")))
 '(org-default-notes-file "~/org/refile.org")
 '(org-combined-agenda-icalendar-file "~/org/agenda.ics")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-tag-alist '(("URGENT" . ?u)
                   ("@CALL" . ?c)
                   ("@ERRANDS" . ?e)))
 '(org-remember-store-without-prompt t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))

