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

(provide 'python-programming)
;;; python-programming.el ends here
