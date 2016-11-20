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

;; Since linters are more clever than me at formatting code, let's use
;; a tool for that, but let's not be too aggresive, just bind it to a
;; key
(use-package py-yapf
  :ensure t
  :config
  (defun my/redef-indent-python ()
    (local-set-key (kbd "C-c TAB") #'py-yapf-buffer))
  (add-hook 'python-mode-hook #'my/redef-indent-python))

;; I liked the pyvenv tool from Elpy. So let's use that. Also, I
;; pretty much use (and will continue to do so) virtualenvwrapper.sh,
;; Thus, the binding that will be used is pyvenv-workon in C-c C-w. We
;; will also need a deactivate key, C-c C-d makes sense.
(use-package pyvenv
  :ensure t
  :config
  (defun my/set-pyvenv-workon ()
    (local-set-key (kbd "C-c C-w") #'pyvenv-workon)
    (local-set-key (kbd "C-c C-d") #'pyvenv-deactivate))
  (add-hook 'python-mode-hook #'my/set-pyvenv-workon))

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
