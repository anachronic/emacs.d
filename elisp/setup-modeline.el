;;; setup-modeline.el --- Mode line configurations
;;; Commentary:
;;; Code:

(defvar ach-valid-pyvenv-modes
  '(python-mode
    projectile-django-server-mode
    projectile-django-migration-mode
    web-mode
    js2-mode
    js2-jsx-mode)
  "Valid modes where we should display the current virtual environment.")

(provide 'setup-modeline)
;;; setup-modeline.el ends here
