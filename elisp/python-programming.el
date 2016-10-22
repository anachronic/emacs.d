;;; python.el --- Python programming
;;; Commentary:
;;; Code:


;; elpy is GREAT!
(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; python-django seems to work quite ok with it.
(use-package python-django
  :ensure t)

(provide 'python-programming)
;;; python-programming.el ends here
