;;; setup-evil.el --- Evil mode.
;;; Commentary:
;;; Code:

(use-package evil
  :commands (evil-mode evil-define-key)
  :demand t
  :config
  (evil-mode 1))

(provide 'setup-evil)
;;; setup-evil.el ends here
