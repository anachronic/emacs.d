;;; python.el --- Python programming
;;; Commentary:
;;; Code:


;; elpy is GREAT!
(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-yasnippet elpy-modules)
  (delete 'elpy-module-flymake elpy-modules))

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
