;;; setup-file-nav.el --- Set up tools to navigate through files.
;;; Commentary:
;;; Code:

;; direx looks like a good alternative. I have tried NeoTree, but it sucks.
(use-package direx
  :ensure t)

;; NeoTree could *sometimes* be better than Dired.
(use-package neotree
  :ensure t
  :bind ("<f7>" . neotree-toggle))

;; This package is cool, I like coloring stuff around
(use-package dired-k
  :ensure t
  :config
  (progn
    (add-hook 'dired-initial-position-hook 'dired-k)
    (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))

;; it seems sensible to hide every detail by default and use ) when I
;; have to show more info, like file size or permissions
(use-package dired+
 :ensure t
 :config
 (global-dired-hide-details-mode 1)
 ;; Since b is unbound in dired and ^ is a very annoying key to press,
 ;; lets bind that to dired-up-directory
 (with-eval-after-load 'dired+
   (define-key dired-mode-map (kbd "b") #'dired-up-directory)))

(provide 'setup-file-nav)
;;; setup-file-nav.el ends here
