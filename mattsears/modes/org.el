;;----------------------------------------------------------------------------
;; Org mode options
;;----------------------------------------------------------------------------

(setq load-path (cons "~/.emacs.d/vendor/org-mode/lisp" load-path))
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp/org-icalendar.el")
(require 'org)
(require 'org-icalendar)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook (lambda ()
                           (local-set-key [(control tab)] 'ibuffer)))

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "~/org/notes.org"))

(defvar org-gtd-file "~/org/appointments.org")
(defvar org-gtd-other-files)
(setf org-gtd-other-files (list
                           "~/org/todos.org"))

(setf org-agenda-files (cons org-gtd-file org-gtd-other-files))

(setq org-tag-alist '(("@home" . ?h)
                      ("@office" . ?o)
                      ("@call" . ?c)
                      ("@errands" . ?e)))

;; Use IDO for target completion
(setq org-completion-use-ido t)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; Default states for todos
(setq org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE")))
(setq org-todo-keyword-faces (quote (("TODO" :foreground "#b9402a" :weight bold)
                                     ("NEXT" :foreground "blue" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold))))

(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
        ("go" "Office" tags-todo "@office")
        ("gc" "Home" tags-todo "@home")
        ("gp" "Phone" tags-todo "@call")
        ("ge" "Errands" tags-todo "@errands")
        ("G" "GTD Block Agenda"
         ((tags-todo "@office")
          (tags-todo "@call")
          (tags-todo "@home")
          (tags-todo "@errands"))
         nil                      ;; i.e., no local settings
         ("~/org/next-actions.html")) ;; exports block to this file with C-c a e
       ;; ..other commands here
        ))

; (setq org-agenda-custom-commands
;      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
;              ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
;              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
;              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
;              ("n" "Notes" tags "NOTE" nil))))

;; Use IDO for auto-complete
(setq org-completion-use-ido t)

;; Do not fold items on startup
(setq org-startup-folded "showall")

;; Use single stars instead
(setq org-hide-leading-stars t)

;; The location of the icalendar fiel
(setq org-combined-agenda-icalendar-file "~/Dropbox/Public/org.ics")
(setq org-icalendar-include-todo t)
(setq org-icalendar-combined-name "Org calendar")
(setq org-icalendar-use-deadline t)
(setq org-icalendar-use-scheduled '(todo-due  event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
(setq org-icalendar-timezone "EST")
(setq org-icalendar-categories (quote (all-tags category todo-state)))
;;(setq org-icalendar-store-UID t)

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

;;----------------------------------------------------------------------------
;; Remember Mode
;;----------------------------------------------------------------------------

(vendor 'remember)
(require 'remember)
(require 'org-remember)
(org-remember-insinuate)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-default-notes-file "~/org/notes.org")
(setq org-archive-location "~/org/archive/%s_archive::")

;; Key binding for remember
(define-key global-map [(control meta ?r)] 'remember)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "New Tasks")
(setq org-refile-targets '( (org-agenda-files :regexp . "New Tasks") ))

(setq org-remember-templates
      '(("Todo" ?t "** TODO %^{Brief Description} %^g" "~/org/todos.org" "New Tasks")
        ("Note" ?n "** %U %?\n\n  %i\n %a" "~/org/notes.org" "Notes")
        ("Idea" ?i "** %^{Title}\n  %i\n  %a" "~/org/ideas.org" "New Ideas")
        ("Appointment" ?a "* %^{Event}\n  SCHEDULED: %^t\n  %i\n  %a" "~/org/appointments.org" "Appointments")
        ))

(defadvice org-publish-projects
  (around org-publish-disable-flymake activate)
  "Disable `flymake' while publishing `org-mode' files."
  (let ((flymake-allowed-file-name-masks))
    ad-do-it))

(defun matts-org-export-icalendar ()
  "Custom export function for icalendars"
  (interactive)
  (if (file-exists-p org-combined-agenda-icalendar-file)
        (delete-file org-combined-agenda-icalendar-file))
  (org-export-icalendar-combine-agenda-files)
)

(defun matts-todos ()
   (interactive)
   (find-file "~/org/todos.org")
 )

(defun matts-appointments ()
  (interactive)
  (find-file "~/org/appointments.org")
  )
