;;; setup-search.el --- Functions to search and jump around..
;;; Commentary:
;;; Code:

;; While I once thought avy was cool, it isn't really what I thought
;; it would be. I'd much rather use isearch and iy-go-up-to-char
(use-package avy
  :ensure t
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-:"     . avy-goto-word-1)))

;; I really feel like this package is too good to bind it to a large
;; key binding.
(use-package iy-go-to-char
  :ensure t
  :after key-chord
  :bind (("C-c j c" . iy-go-up-to-char)
         ("C-c j b" . iy-go-to-char-backward))
  :config
  (require 'key-chord)
  (key-chord-define-global "jk" #'iy-go-to-char-backward)
  (key-chord-define-global "kl" #'iy-go-up-to-char)
  (global-set-key (kbd "C-c j c") #'iy-go-up-to-char)
  (global-set-key (kbd "C-c j b") #'iy-go-to-char-backward)

  ;; Having a cool keyboard layout allows me to do things like these
  (global-set-key (kbd "ï") #'iy-go-to-or-up-to-continue)
  (global-set-key (kbd "œ") #'iy-go-up-to-char-continue-backward))

;; Seems better than ace-link
(use-package link-hint
  :ensure t
  :bind ("C-c j u" . link-hint-open-link)
  :demand
  :config
  (defun my/set-link-hint-key ()
    (local-set-key "o" #'link-hint-open-link))
  (add-hook 'package-menu-mode-hook #'my/set-link-hint-key)
  (add-hook 'help-mode-hook #'my/set-link-hint-key))

;; I've been using occur pretty frequently.
(global-set-key (kbd "C-S-o") 'occur)

;; I gave it a try. looks pretty cool.
(use-package anzu
  :ensure t
  :diminish ""
  :config
  ;; Spaceline already has the config. So let's remove anzu's modeline toggle.
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp))

;; Lets use swiper conservatively. I actually like isearch better.
(use-package swiper
  :ensure t
  :bind ("C-S-s" . swiper)
  :config
  ;; I'd like to do something along the lines of isearch-swiper, which
  ;; means to switch my isearch query to swiper
  (defun isearch-swiper ()
    "Switch from isearch to swiper keeping query string."
    (interactive)
    (isearch-exit)
    (anzu--reset-mode-line)
    (swiper isearch-string))
  (define-key isearch-mode-map (kbd "C-S-s") #'isearch-swiper)
  (define-key isearch-mode-map (kbd "C-S-r") #'isearch-swiper))

;; This is GREAT when tags don't really cut it
(use-package dumb-jump
  :ensure t
  :diminish (dumb-jump-mode . "Dumb")
  :config
  (add-hook 'prog-mode-hook 'dumb-jump-mode))



(provide 'setup-search)
;;; setup-search.el ends here
