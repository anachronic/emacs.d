;;; setup-file-nav.el --- Set up tools to navigate through files.
;;; Commentary:
;;; Code:

;; direx looks like a good alternative. I have tried NeoTree, but it sucks.
(use-package direx
  :ensure t
  :bind ("C-x C-j" . direx:jump-to-directory))

;; NeoTree could *sometimes* be better than Dired.
(use-package neotree
  :ensure t
  :bind ("<f7>" . neotree-toggle))

;; Since b is unbound in dired and ^ is a very annoying key to press,
;; lets bind that to dired-up-directory
(define-key dired-mode-map (kbd "b") #'dired-up-directory)



;;; setup-file-nav.el ends here
