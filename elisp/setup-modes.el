;;; setup-modes.el --- Modes that don't require separate files.
;;; Commentary:
;;; Code:

;; yaml-mode. mainly for syntax highlighting
(use-package yaml-mode
  :ensure t ;; seems like overkill
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package camcorder
  :defer t)

;; not sure where to put this
(use-package feature-mode
  :ensure t)

;; this one is builtin
(use-package subword
  :defer t
  :diminish "w")

(provide 'setup-modes)
;;; setup-modes.el ends here
