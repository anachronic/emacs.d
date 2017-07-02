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

;; I gave it a try. looks pretty cool.
(use-package anzu
  :ensure t
  :diminish ""
  :config
  ;; Spaceline already has the config. So let's remove anzu's modeline toggle.
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp))

;; Lets use swiper conservatively. I actually like isearch better.
(use-package swiper
  :ensure t
  :bind (("C-c s" . swiper)
         ("C-S-s" . swiper))
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
  (define-key isearch-mode-map (kbd "C-S-r") #'isearch-swiper)
  (define-key isearch-mode-map (kbd "C-c s") #'isearch-swiper)
  ;; Terminal key bindings
  (define-key meta-m-map (kbd "M-s") #'swiper))

;; This is GREAT when tags don't really cut it
(use-package dumb-jump
  :ensure t
  :diminish (dumb-jump-mode . "Dumb")
  :config
  (define-key dumb-jump-mode-map (kbd "C-M-g") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-p") nil)
  (define-key dumb-jump-mode-map (kbd "C-M-q") nil)
  (define-key dumb-jump-mode-map (kbd "M-g j") 'dumb-jump-go)
  (define-key dumb-jump-mode-map (kbd "M-g b") 'dumb-jump-back)
  (define-key dumb-jump-mode-map (kbd "M-g q") 'dumb-jump-quick-look)
  (add-hook 'prog-mode-hook 'dumb-jump-mode))

(provide 'setup-search)
;;; setup-search.el ends here
