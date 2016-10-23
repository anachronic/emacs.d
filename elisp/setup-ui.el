;;; setup-ui.el --- UI related configuration.
;;; Commentary:
;;; Code:

;; get rid of everything other than the title at the top.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

;; load the latest theme.
(load-theme 'avk-darkblue-white t)
(set-face-attribute 'default nil :height 105)

(use-package spaceline
  :ensure t
  :demand
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq spaceline-minor-modes-separator " "))

;; Frame commands
(global-set-key (kbd "C-c F") 'make-frame)
(global-set-key (kbd "C-c K") 'delete-frame)


;; no DING!
(setq visible-bell 1)

;; This has been driving me crazy. So new key binding.
(global-set-key (kbd "<f9>") 'split-window-right)

;; enlarge/shrink current window
(global-set-key (kbd "s-f") 'enlarge-window-horizontally)
(global-set-key (kbd "s-b") 'shrink-window-horizontally)


(provide 'setup-ui)
;;; setup-ui.el ends here
