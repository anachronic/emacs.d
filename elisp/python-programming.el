;;; python.el --- Python programming
;;; Commentary:
;;; Code:

;; Switch to anaconda-mode from Elpy
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook #'anaconda-mode))

;; Use company anaconda for completions. capf seems REALLY slow
(use-package company-anaconda
  :ensure t
  :after anaconda-mode
  :config
  (defun my/add-company-anaconda ()
    (setq-local company-backends company-backends)
    (add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook #'my/add-company-anaconda))

;; python-django seems to work quite ok with it.
(use-package python-django
  :ensure t)

;; For some reason color identifiers mode doesn't start with Python I
;; guess either Python is not a prog-mode or Elpy doesn't play nice
;; with it. Whatever
(defun my/add-color-identifiers-mode ()
  "Add color identifiers mode to the buffer or whatever."
  (color-identifiers-mode 1))
(add-hook 'python-mode-hook #'my/add-color-identifiers-mode)

(provide 'python-programming)
;;; python-programming.el ends here
