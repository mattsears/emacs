(use-package hydra
  :defer 0.5
  :bind (("C-c m" . hydra-magit/body)
         ("C-c b" . hydra-bufffer/body)))

(defhydra hydra-magit (:color blue)
  "
  ^
  ^Magit^             ^Do^                             ^View^
  ^─────^─────────────^──^─────────────────────────────^──^───────────────────────────
  _q_ quit            _s_ status                       _b_ blame
  ^^                  _n_ create new branch            _l_ view full log
  ^^                  _c_ commit                       _t_ view log for this buffer
  ^^                  _k_ checkout
  ^^                  _p_ pull
  ^^                  _o_ browse remote
  ^^                  _f_ spinoff new branch
  ^^
  ^^                  ^^
  "
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-commit-create)
  ("n" magit-branch-create)
  ("t" magit-log-buffer-file)
  ("l" magit-log-all)
  ("k" magit-branch-checkout)
  ("p" magit-pull-from-upstream)
  ("o" browse-at-remote)
  ("f" magit-branch-spinoff)
  ("s" magit-status)
  )

(defhydra hydra-buffer (:color blue)
  "
  ^
  ^Buffer^             ^Do^
  ^──────^─────────────^──^──────────
  _q_ quit             _k_ kill
  ^^                   _l_ list
  ^^                   _n_ next
  ^^                   _p_ previous
  ^^                   _e_ eval this buffer
  ^^                   ^^
  "
  ("q" nil)
  ("k" kill-buffer)
  ("l" ibuffer)
  ("n" next-buffer)
  ("e" eval-buffer)
  ("p" previous-buffer))

(defhydra hydra-files (:color blue)
  "
  ^
  ^Files^             ^Do^
  ^──────^─────────────^──^──────────
  _q_ quit             _d_ delete
  ^^                   _r_ rename
  ^^                   _m_ move
  ^^                   _y_ create new directory
  ^^                   _o_ open in finder
  ^^                   _e_ open project .env file
  ^^                   ^^
  "
  ("q" nil)
  ("d" matts/delete-this-buffer-and-file)
  ("e" matts/open-env)
  ("m" matts/move-file)
  ("y" make-directory)
  ("r" matts/rename-file-and-buffer)
  ("o" reveal-in-folder)
  )

(defhydra hydra-rails (:color blue)
  "
  ^
  ^ Rails^             ^Find^                         ^Goto^                               ^Do^
  ^──────^─────────────^──^───────────────────────────^──^─────────────────────────────────^──^───────────
  _q_ quit             _m_ find model                 _h_ goto schema                       _n_ generate
  ^^                   _c_ find controler             _r_ goto routes                       _p_ extact partial
  ^^                   _v_ find view                  _G_ goto Gemfile                      _:_ convert to symbol
  ^^                   _t_ find test file             _C_ find current controller           _'_ convert to single quote
  ^^                   _g_ find migration             _M_ find current model                _x_  restart the server
  ^^                   _s_ find spec                  _1_ goto this model
  ^^                   _j_ find javascript            _2_ goto this controller
  ^^                   _l_ find lib                   _3_ goto this test
  ^^                   _y_ find layout                _4_ goto this migration
  ^^                   ^^
  "
  ("q" nil)
  ("m" projectile-rails-find-model)
  ("c" projectile-rails-find-controller)
  ("v" projectile-rails-find-view)
  ("t" projectile-find-test-file)
  ("s" projectile-rails-find-spec)
  ("h" projectile-rails-goto-schema)
  ("r" projectile-rails-goto-routes)
  ("x" matts/restart-rails-server)
  ("g" projectile-rails-find-migration)
  ("G" projectile-rails-goto-gemfile)
  ("p" projectile-rails-extract-region)
  ("y" projectile-rails-find-layout)
  ("j" projectile-rails-find-javascript)
  ("l" projectile-rails-find-lib)
  ("C" projectile-rails-find-current-controller)
  ("M" projectile-rails-find-current-model)
  ("n" projectile-rails-generate)
  ("1" projectile-rails-find-current-model)
  ("2" projectile-rails-find-current-controller)
  ("3" projectile-rails-find-current-test)
  ("4" projectile-rails-find-current-migration)
  (":" ruby-tools-to-symbol)
  ("'" ruby-tools-to-single-quote-string)
  )

(defhydra hydra-markdown (:color blue)
  "
  ^
  ^Mardown^           ^Insert^                         ^Header^
  ^─────^─────────────^──^─────────────────────────────^──^───────────────────────────
  _q_ quit            _b_ insert bold                  _1_ insert header 1
  ^^                  _i_ insert italic                _2_ insert header 2
  ^^                  _q_ insert blockquote            _3_ insert header 3
  ^^                  _c_ insert code                  _4_ insert header 4
  ^^                  _l_ insert link
  ^^                  _i_ insert list item
  ^^                  ^^
  "
  ("b" markdown-insert-bold)
  ("i" markdown-insert-italic)
  ("q" markdown-insert-blockquote)
  ("c" markdown-insert-code)
  ("l" markdown-insert-link)
  ("i" markdown-insert-list-item)

  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ;; ("l" markdown-promote)
  ;; ("r" markdown-demote)
  )


(defhydra hydra-org (:color blue)
  "
  ^
  ^Orgmode^           ^Insert^                         ^Tables^
  ^─────^─────────────^──^─────────────────────────────^──^───────────────────────────
  _q_ quit            _h_ insert heading               _t_ insert table
  ^^                  _s_ insert subheading            _r_ insert row
  ^^                  _l_ insert link                  _a_ align rows
  ^^                  _e_ emphasize
  ^^                  _x_ export to markdown
  ^^
  ^^                  ^^
  "

  ("q" nil)
  ("h" org-insert-heading)
  ("s" org-insert-subheading)
  ("l" org-insert-link)
  ("e" org-emphasize)
  ("x" org-md-export-to-markdown)

  ;; Tables
  ("t" org-table-create)
  ("r" org-table-insert-row)
  ("a" org-table-align)
  )

(provide 'config-menus)
