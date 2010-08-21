;;----------------------------------------------------------------------------
;; ERC Settings
;;----------------------------------------------------------------------------
(setq erc-autojoin-channels-alist '(("freenode.net"))
      erc-nick "mattsears"
      erc-server "irc.freenode.net"
      erc-away-nickname "mattsears_AWAY"
      erc-user-full-name "Matt Sears")

(setq erc-prompt (lambda ()
                   (if (and (boundp 'erc-default-recipients) (erc-default-target))
                       (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
                     (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t))))

;;(setq erc-autojoin-channels-alist
;;   '(("freenode.net" "#littlelines" )))