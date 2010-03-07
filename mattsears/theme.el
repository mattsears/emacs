;; Pretty colors
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(load-file "~/.emacs.d/vendor/color-theme-neptune.el")
(color-theme-neptune)

;; Set the default font-size to 16pt
(set-face-attribute 'default nil :height 150)

(setq transparency-level 85)

;; turn off 3d modeline
(set-face-attribute 'mode-line nil :box nil)

;; Modify the mode-line as well. This is a cleaner setup than the default
; (setq default-mode-line-format
;       '(" "
;         mode-line-frame-identification
;         mode-line-buffer-identification
;         "  "
;         global-mode-string
;         "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]  "
;         (line-number-mode "Line %l  ")
;         (column-number-mode "Column %c  ")
;         (-3 . "%p")
;         "% "))

;;----------------------------------------------------------------------------
;; Ruby specific color hacks
;;----------------------------------------------------------------------------

(add-hook 'ruby-hook
  (lambda ()
   (font-lock-add-keywords nil
    '(
	  ("(\\(lambda\\)\\>" (0 (prog1 ()
	                             (compose-region (match-beginning 1)
	                                             (match-end 1)
	                                             ?λ))))
      ("\\<\\(private\\)" 1 font-lock-function-name-face prepend)
      ("\\<\\(protected\\)" 1 font-lock-function-name-face prepend)
      ("\\<\\(require\\)" 1 font-lock-function-name-face prepend)
   ))))

(defvar ruby-functions
  '("private"
    "protected"
    "require"
    )
  "Ruby keywords")

;;----------------------------------------------------------------------------
;; Hack: Add color to angle brackets and strings in html
;;----------------------------------------------------------------------------

(defvar html-mode-keywords
  '(("\\(<[^>]+>\\)" 1 font-lock-variable-name-face prepend)
    ("\\(\"[^\"]*\"\\)" 1 font-lock-string-face prepend)
    ("\\('[^']*'\\)" 1 font-lock-string-face prepend)))

(font-lock-add-keywords 'rhtml-mode html-mode-keywords)
(font-lock-add-keywords 'html-mode html-mode-keywords)
(font-lock-add-keywords 'html-helper-mode html-mode-keywords)

;;----------------------------------------------------------------------------
;; Diff mode cosmetics
;;----------------------------------------------------------------------------

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "#9fab7d")
     (set-face-foreground 'diff-removed "#CF6A4C")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "#9fab7d")
     (set-face-foreground 'magit-diff-del "#CF6A4C")))

;;----------------------------------------------------------------------------
;; Rails specific color hacks
;;----------------------------------------------------------------------------

(defvar ruby-rails-functions
  '("add_column"
    "after_create"
    "after_destroy"
    "after_filter"
    "after_generate"
    "after_save"
    "after_validation"
    "after_update"
    "before_create"
    "before_destroy"
    "before_filter"
    "before_save"
    "before_update"
    "belongs_to"
    "belongs_to?"
    "change_column"
    "check_box"
    "content_for"
    "date_select"
    "datetime_select"
    "error_messages"
    "file_field"
    "has_many"
    "has_and_belongs_to_many"
    "image_tag"
    "label"
    "link_to"
    "link_to_function"
    "link_to_if"
    "link_to_remote"
    "link_to_unless"
    "link_to_unless_current"
    "render"
    "submit"
    "text_area"
    "text_field"
    "validate"
    "validate_digest_response"
    "validate_nonce"
    "validate_on_update"
    "validates_acceptance_of"
    "validates_associated"
    "validates_confirmation_of"
    "validates_each"
    "validates_exclusion_of"
    "validates_format_of"
    "validates_inclusion_of"
    "validates_length_of"
    "validates_numericality_of"
    "validates_presence_of"
    "validates_size_of"
    "validates_uniqueness_of"
    )
  "Rails keywords")

(defface rails-font-lock-function-face
  '((t (:inherit neptune-light-blue)))
  "Rails functions"
  :group 'rails-font-lock-faces)

(defvar rails-font-lock-function-face 'rails-font-lock-function-face
  "Face name for rails function font")

(setq rhtml-in-erb-keywords
      (append rhtml-in-erb-keywords
              (list (list
                     (concat "[^_]"
                             (regexp-opt ruby-rails-functions 'words)
                             "[^_]")
                     1 rails-font-lock-function-face 'prepend)
                    '("[^$\\?]\\(\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"\\)"
                      . (1 font-lock-string-face prepend))
                    '("[^$\\?]\\('[^\\']*\\(\\\\\\(.\\|\n\\)[^\\']*\\)*'\\)"
                      . (1 font-lock-string-face prepend)))))

;; Hack: Add color to strings in ERB
;; (add-to-list 'rhtml-in-erb-keywords '("\\(#{[^>]*}\\)" .
;;                                       (1 font-lock-doc-face prepend)) )
;; (add-to-list 'rhtml-in-erb-keywords '("\\(<!--[^>]*-->\\)" .
;;                                       (1 font-lock-comment-face prepend)) )
;; (add-to-list 'rhtml-in-erb-keywords '("\\(\"[^>]*\"\\)" .
;;                                       (1 font-lock-string-face prepend)) )
;; (add-to-list 'rhtml-in-erb-keywords '("\\(\'[^>]*\'\\)" .
;;                                       (1 font-lock-string-face prepend)) )
