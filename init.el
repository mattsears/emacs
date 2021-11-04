;;----------------------------------------------------------------------------
;; Global Package Initializations
;;----------------------------------------------------------------------------

(setq load-prefer-newer t)
(setq warning-minimum-level :emergency)

(if (member "-M" command-line-args)

    (require 'cl-lib)

  (cl-letf* (;; In fact, never GC during initialization to save time.
             (gc-cons-threshold 402653184)
             (gc-cons-percentage 0.6)
             (file-name-handler-alist nil)
             (load-source-file-function nil)

             ;; Also override load to hide  superfluous loading messages
             (old-load (symbol-function #'load))
             ((symbol-function #'load)
              (lambda (file &optional noerror _nomessage &rest args)
                (apply old-load
                       file
                       noerror
                       (not (eq debug-on-error 'startup))
                       args))))

    (message "[               ]")

    (eval-and-compile
      (add-to-list 'load-path (locate-user-emacs-file "modules/")))

    ;; suppress the GNU spam
    (fset 'display-startup-echo-area-message #'ignore)
    (add-hook 'emacs-startup-hook (lambda () (message "")))

    ;; Use-package loads packages only when packages are loaded
    (require 'cask "~/.cask/cask.el")
    (cask-initialize)
    ;;(require 'use-package)
    ;;(require 'ruby-block)
    (require 'config-defuns)
    (message "[=              ] utilities")
    (require 'config-setq)
    (message "[=              ] setq")
    (require 'config-ui)
    (message "[==             ] ui")
    (require 'config-shell)
    (message "[===            ] shell")
    (require 'config-ruby)
    (message "[====           ] ruby")
    (require 'config-evil)
    (message "[======         ] evil")
    (require 'config-magit)
    (message "[=======        ] magit")
    (require 'config-modes)
    (message "[=========      ] modes")
    (require 'config-theme)
    (message "[===========    ] color theme")
    (require 'config-webdev)
    (message "[=============  ] web dev")
    (require 'config-menus)
    (message "[============== ] menus")
    (require 'config-packages)
    (message "[===============] packages...")
    ))
