;;----------------------------------------------------------------------------
;; Css mode
;;----------------------------------------------------------------------------
(require 'css-mode)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(defadvice cssm-complete-property
  (after cssm-complete-add-space activate)
  "Modify CSS completion to add a space after full completion."
  (when (eq (char-before) ?:) (insert " ")))

(define-skeleton cssm-insert-semicolon
  "Inserts a semicolon." nil
  ";" "\n" >)

(defadvice cssm-enter-mirror-mode
  (after cssm-enter-mirror-semicolon activate)
  "Add electric semicolons to css-mode's \"mirror mode.\""
  (define-key cssm-mode-map (read-kbd-macro ";")  'cssm-insert-semicolon))

(defadvice cssm-leave-mirror-mode
  (after cssm-leave-mirror-semicolon activate)
  "Add electric semicolons to css-mode's \"mirror mode.\""
  (define-key cssm-mode-map (read-kbd-macro ";")  'self-insert-command))

;; Highlights HTML/CSS color specifications
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
     (0 (let ((colour (match-string-no-properties 0)))
          (if (or (= (length colour) 4)
                  (= (length colour) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)
                           :foreground (if (>= (apply '+ (x-color-values
                                                          (match-string-no-properties 0)))
                                               (* (apply '+ (x-color-values "white")) .6))
                                           "black" ;; light bg, dark text
                                         "white" ;; dark bg, light text
                                         )))))
        append))))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords t))

(add-hook 'css-mode-hook
          'hexcolour-add-to-font-lock
          '(lambda ()
             (setq cssm-indent-level 2)
             (setq cssm-newline-before-closing-bracket t)
             (setq cssm-indent-function #'cssm-c-style-indenter)
             (setq cssm-mirror-mode nil)
             (setq tab-width 2)
             (define-key css-mode-map [return] 'newline-and-indent)
             (setq css-electric-brace-behavior t)
             (setq css-electric-semi-behavior t)
             (setq css-indent-offset 2)))
