;;; js-programming.el --- Javascript programming relevant stuff.
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :commands (js2-mode js2-jsx-mode)
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2)
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

(use-package tern
  :ensure t
  :after js2
  :defer t
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :ensure t
  :after (company tern js2-mode)
  :defer t
  :config
  (defun my/add-tern-company ()
    (setq-local company-backends company-backends)
    (add-to-list 'company-backends 'company-tern))
  (add-hook 'js2-mode-hook #'my/add-tern-company))


;; Been using this one for work lately
(use-package json-reformat
  :ensure t)

(defun my/add-keychords-js2-mode ()
  "Add ;; keystroke to mean insert semicolon at the end of the line."
  (key-chord-define-local ";;" "\C-e;"))

(add-hook 'js2-mode-hook 'my/add-keychords-js2-mode)

;; javascript is a 2-spaces indenting language.
(add-hook 'js2-mode-hook #'indent-guide-mode)

(provide 'js-programming)
;;; js-programming.el ends here
