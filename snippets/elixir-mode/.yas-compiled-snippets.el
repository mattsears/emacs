;;; Compiled snippets and support files for `elixir-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'elixir-mode
                     '(("pry" "require IEx; IEx.pry $0" "" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/elixir-mode/pry" nil nil)
                       ("po" "|> $0" "po" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/po" nil nil)
                       ("if" "if ${1:true} do\n$0\nend" "if" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/if" nil nil)
                       ("for" "for ${2:x} <- ${1:Enumeration}, do: $2$0" "for" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/for" nil nil)
                       ("fn" "fn ${1:}-> ${0:$1}end" "fn" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/fn" nil nil)
                       ("doc" "@doc \"\"\"\n  $0\n\"\"\"" "doc" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/doc" nil nil)
                       ("do" "do\n$0\nend" "do" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/do" nil nil)
                       ("defp" "defp ${1:function(args)} do\n$0\nend" "defp" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/defp" nil nil)
                       ("def" "def ${1:function(args)} do\n$0\nend" "def" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/def" nil nil)
                       ("cond" "cond do\n$0\nend" "cond" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/cond" nil nil)
                       ("case" "case ${1:true} do\n$0\nend" "case" nil nil nil "/Users/matt/.emacs.d/snippets/elixir-mode/case" nil nil)))


;;; Do not edit! File generated at Fri Dec 17 15:11:50 2021
