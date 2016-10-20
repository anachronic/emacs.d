;; get rid of everything other than the title at the top.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

;; load the latest theme.
(load-theme 'avk-darkblue-white t)

(use-package spaceline
  :ensure t
  :demand
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; been using frames lately, so let's make a keybinding for that
(global-set-key (kbd "C-c F") 'make-frame)

;; no DING!
(setq visible-bell 1)


(provide 'setup-ui)
;;; setup-ui.el ends here
