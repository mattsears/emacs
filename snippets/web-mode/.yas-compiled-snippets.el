;;; Compiled snippets and support files for `web-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'web-mode
                     '(("unless" "<% unless ${cond} -%>\n$0\n<% end -%>\n" "<% unless  ...  -%> $. <% end -%>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/unless" nil nil)
                       ("tf" "<%= text_field(\"${object_name} \", \"${method}\"$0) %>" "<%= text_field(\" ... \", \" ... \") %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/tf" nil nil)
                       ("td" "<td>$1</td>" "<td>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/td" nil nil)
                       ("ta" "<%= text_area(\"${object_name}\", \"${method}\"$0) %>\n" "<%= text_area(\" ... \", \" ... \") %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/ta" nil nil)
                       ("t" "<%= t('$0') %>" "<%= t('') %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/t" nil nil)
                       ("strong" "<strong>$0</strong>\n" "<strong>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/strong" nil nil)
                       ("span" "<span>$1</span>" "<span>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/span" nil nil)
                       ("rp" "<%= render(partial: \"${action}\"$0 ) %>" "<%= render(:partial => ... ) %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/rp" nil nil)
                       ("row" "<div class=\"row\">\n  $0\n</div>" "<row>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/row" nil nil)
                       ("p" "<p>$0</p>\n" "<p>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/p" nil nil)
                       ("lt" "<%= link_to \"${title}\", ${index}_path %>$0" "<%= link_to \" ... \", :action => \" ... \" %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/lt" nil nil)
                       ("licai" "<%= link_to \"${title}\", :controller => \"${items}\", :action => \"${edit}\", :id => ${@item} %>$0" "<%= link_to \" ... \", :controller => \" ... \", :action => \" ... \", :id =>  ...  %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/licai" nil nil)
                       ("lica" "<%= link_to \"${title}\", :controller => \"${items}\", :action => \"${index}\" %>$0" "<%= link_to \" ... \", :controller => \" ... \", :action => \" ... \" %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/lica" nil nil)
                       ("lic" "<%= link_to \"${title}\", :controller => \"${items}\" %>$0" "<%= link_to \" ... \", :controller => \" ... \" %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/lic" nil nil)
                       ("liai" "<%= link_to \"${title}\", :action => \"${edit}\", :id => ${@item} %>$0" "<%= link_to \" ... \", :action => \" ... \", :id =>  ...  %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/liai" nil nil)
                       ("li" "<li class=\"${class}\">\n  $0\n</li>\n" "<li>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/li" nil nil)
                       ("lb" "<%= f.label :$0, class: \"$1\" %>\n" "<%= label :action, class: 'small' %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/lb" nil nil)
                       ("img" "<%= image_tag \"${name}\", class: \"${class}\", alt: \"${alt}\" %>$0\n" "<%= image_tag \" ... \", :action => \" ... \" %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/img" nil nil)
                       ("ifel" "<% if $0cond} -%>\n$0\n<% else -%>\n<% end -%>\n" "<% if  ...  -%> $. <% else -%> <% end -%>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/ifel" nil nil)
                       ("if" "<% if ${false} -%>\n  $0\n<% end -%>\n" "<% if  ...  -%> $. <% end -%>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/if" nil nil)
                       ("h" "<%=h ${@item} %>\n" "<%=h  ...  %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/h" nil nil)
                       ("ft" "<% form_tag(:action => \"${update}\") do %>\n$0\n<% end %>\n" "<% form_tag(:action => \" ... \") do %> ... <% end  %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/ft" nil nil)
                       ("end" "<% end %>" "end" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/end" nil nil)
                       ("em" "<em>$0</em>" "<em>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/em" nil nil)
                       ("else" "<% else %>" "else" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/else" nil nil)
                       ("div" "<div class=\"${class}\">\n  $0\n</div>" "<div>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/div" nil nil)
                       ("col" "<div class=\"col\">\n  $0\n</div>" "<col>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/col" nil nil)
                       ("br" "<br />" "<br>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/br" nil nil)
                       ("b" "<b>$0</b>" "<b>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/b" nil nil)
                       ("=" "<%= $0 %>" "<%=$. %>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/=" nil nil)
                       ("%" "<%$0 -%>\n" "<%$. -%>" nil nil nil "/Users/matt/.emacs.d/snippets/web-mode/%" nil nil)))


;;; Do not edit! File generated at Fri May 28 10:02:39 2021
