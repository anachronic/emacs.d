;;; setup-search.el --- Functions to search and jump around..
;;; Commentary:
;;; Code:

;; Seems better than ace-link
(use-package link-hint
  :ensure t
  :bind ("C-c v" . link-hint-open-link)
  :demand
  :config
  (defun ach-set-link-hint-key ()
    (local-set-key "o" #'link-hint-open-link))
  (add-hook 'package-menu-mode-hook #'ach-set-link-hint-key)
  (add-hook 'help-mode-hook #'ach-set-link-hint-key))

;; Lets use swiper conservatively. I actually like isearch better.
(use-package swiper
  :ensure t
  :init
  (define-key evil-normal-state-map (kbd "C-s") 'swiper))

;; evil-anzu
(use-package evil-anzu
  :ensure t
  :defer t
  :init
  (setq-default anzu-cons-mode-line-p nil)
  (with-eval-after-load 'evil
    (require 'evil-anzu)))

;; This is GREAT when tags don't really cut it
(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg)
  (define-key dumb-jump-mode-map (kbd "C-M-g") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-q") nil)
  (add-hook 'prog-mode-hook 'dumb-jump-mode))

(provide 'setup-search)
;;; setup-search.el ends here
