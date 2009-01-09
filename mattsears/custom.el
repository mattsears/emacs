; Set custom variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 162 t)
 '(aquamacs-save-options-on-quit nil)
 '(default-frame-alist (quote ((tool-bar-lines . 0) (fringe) (right-fringe) (left-fringe . 1) (vertical-scroll-bars) (cursor-type . box) (menu-bar-lines . 0) (background-color . "#0C1021") (background-mode . dark) (border-color . "#dedede") (cursor-color . "#AFAFAF") (foreground-color . "#F8F8F8") (mouse-color . "sienna1"))))
 '(load-home-init-file t t)
 '(nxml-slash-auto-complete-flag t)
 '(show-paren-mode nil)
 '(tabbar-mode nil nil (tabbar))
 '(transient-mark-mode t)
 '(aquamacs-autoface-mode nil)
 '(x-stretch-cursor t))

;;  The only I can get a bar cursor on mac os
(setq initial-frame-alist
      (cons '(cursor-type . bar)(copy-alist initial-frame-alist)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit prog-mode-default :slant normal :weight normal :height 120 :family "monaco"))))
 '(text-mode-default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))) t))
