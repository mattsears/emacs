;; Ido mode
(require 'ido)

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-filename-at-point t
      ido-max-prospects 20)

(setq ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer" "^\\*Ibuffer"
        "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-" "^\\*Minibuf-\\*"
        "_region_" " output\\*$" "^TAGS$" "^\*Ido")
      ido-ignore-directories
      '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
      ido-ignore-files
      '("\\`auto/" "\\.prv/" "\\.pyc/"  "\\.class/" "_region_" "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"))


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

(add-to-list
 'ibuffer-fontification-alist
 '(5 (memq major-mode
           '(ruby-mode html-mode emacs-lisp-mode rhtml-mode css-mode
                      javascript-mode))
     font-lock-doc-face))

(add-to-list
 'ibuffer-fontification-alist
 '(5 (memq major-mode
           '(erc-mode))
     font-lock-keyword-face))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("irc" (mode . erc-mode))))))

;; key bindings
(define-key ibuffer-mode-map [delete] 'ignore)

(add-hook 'ibuffer-mode-hooks
          '(lambda ()
             ;; hide any buffers with asterisks
             (ibuffer-switch-to-saved-filter-groups "default")
             (ibuffer-add-to-tmp-hide "^\\*")))
