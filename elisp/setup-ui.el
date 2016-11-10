;;; setup-ui.el --- UI related configuration.
;;; Commentary:
;;; Code:

;; get rid of everything other than the title at the top.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; these two come from: https://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

;; also get rid of the scratch buffer message
(setq initial-scratch-message nil)

;; load the latest theme.
(load-theme 'monokai t)
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

;; I like next/previous buffer handy. I don't always want to write
;; the name of the buffer i'm looking for.
(global-set-key (kbd "C-<tab>") 'previous-buffer)
(global-set-key (kbd "<f10>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)

;; This mode looks way cool. Let's use it
;; We shall never lose the cursor again.!
;; URL: http://endlessparentheses.com/beacon-never-lose-your-cursor-again.html
(use-package beacon
  :ensure t
  :config
  (beacon-mode)
  (setq beacon-push-mark 35)
  (setq beacon-color "#F92672")
  :diminish 'beacon-mode)

(provide 'setup-ui)
;;; setup-ui.el ends here
