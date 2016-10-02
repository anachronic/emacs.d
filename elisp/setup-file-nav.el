;;; setup-file-nav.el --- Set up tools to navigate through files.
;;; Commentary:
;;; Code:

;; direx looks like a good alternative. I have tried NeoTree, but it sucks.
(use-package direx
  :ensure t
  :bind ("C-x C-j" . direx:jump-to-directory))
;;; setup-file-nav.el ends here
