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

(use-package telephone-line
  :ensure t
  :config
  (telephone-line-defsegment ach--telephone-line-narrow-segment ()
    "%n ")
  (setq telephone-line-lhs
        '((evil telephone-line-evil-tag-segment)
          (nil telephone-line-buffer-segment)
          (accent telephone-line-process-segment telephone-line-projectile-segment)
          (nil telephone-line-vc-segment)))
  (setq telephone-line-rhs
        '((nil telephone-line-flycheck-segment telephone-line-misc-info-segment)
          (accent telephone-line-major-mode-segment)
          (evil ach--telephone-line-narrow-segment telephone-line-position-segment)))
  (telephone-line-mode 1))

(provide 'setup-modeline)
;;; setup-modeline.el ends here
