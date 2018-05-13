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

;; this one is builtin
(use-package subword
  :defer t
  :diminish "")

(provide 'setup-modes)
;;; setup-modes.el ends here
