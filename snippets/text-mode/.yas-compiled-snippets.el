;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("before" "console.log(\"I'm here...\");" "before" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/text-mode/cl" nil nil)
                       ("cbox" "$1\n$0" "comment block" nil nil
                        ((yas-after-exit-snippet-hook
                          (lambda nil
                            (if
                                (buffer-substring yas-snippet-beg yas-snippet-end)
                                (comment-box yas-snippet-beg yas-snippet-end 1)))))
                        "/Users/matt/.emacs.d/snippets/text-mode/cbox" nil nil)))


;;; Do not edit! File generated at Fri Dec 17 15:11:50 2021
