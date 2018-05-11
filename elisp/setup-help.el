;;; setup-help.el --- Help enhancements.
;;; Commentary:
;;; Code:

;; I use `describe-face' quite often, I'd really like a key for it
(define-key help-map (kbd "C-f") nil)
(define-key help-map (kbd "C-f") #'counsel-describe-face)

;; Command-log is useful sometimes, but let's not autoinstall as it
;; will only slow first run time.
(use-package command-log-mode
  :defer t)

;; which-key seems like a really nice help
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :demand
  :diminish which-key-mode)

(provide 'setup-help)
;;; setup-help.el ends here
