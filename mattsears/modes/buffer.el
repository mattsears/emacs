;; Ignore certain files when switching buffers
(require 'iswitchb)
(add-to-list 'iswitchb-buffer-ignore "^ ")
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*ECB")
(add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "*ftp ")
(add-to-list 'iswitchb-buffer-ignore "*bsh")
(add-to-list 'iswitchb-buffer-ignore "*jde-log")
(add-to-list 'iswitchb-buffer-ignore "^[tT][aA][gG][sS]$")

;; ibuffer options
(require 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(setq ibuffer-formats
      '((mark modified read-only " " (name 30 30)
              " " (size 10 -1) " " (mode 20 20) " " filename)
        (mark " " (name 30 -1) " " filename)))

(setq ibuffer-never-show-predicates
      (list "\\*Completions\\*"
            "\\*Messages\\*"
            "\\*scratch\\*"
            "\\*Backtrace\\*"
            "\\*vc\\*"))

;; default groups for ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (mode . org-mode))
               ("ruby" (mode . ruby-mode))
               ("html" (mode . nxml-mode))
               ("css" (mode . css-mode))
               ("sass" (mode . sass-mode))
               ("yaml" (mode . yaml-mode))
               ("haml" (mode . haml-mode))
               ("javascript" (mode . javascript-mode))
               ("emacs" (or
                         (name . "^\\*el\\*$")
                         (name . "^\\*Messages\\*$")))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))))))

;;ibuffer, I like my buffers to be grouped
 (add-hook 'ibuffer-mode-hook
           (lambda ()
             (ibuffer-switch-to-saved-filter-groups
              "default")))
