;;; js-programming.el --- Javascript programming relevant stuff.
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :pin gnu
  :commands (js2-mode js2-jsx-mode)
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

(use-package tern
  :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-tern)))

(provide 'js-programming)
;;; js-programming.el ends here
