;; get rid of everything other than the title at the top.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

(provide 'setup-ui)
;;; setup-ui.el ends here
