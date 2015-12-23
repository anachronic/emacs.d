;;; package --- Summary
;;; Commentary:
;;; Setting up emacs for python coding.
;;; Code:
(require 'py-autopep8)

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(provide 'setup-python)
;;; setup-python.el ends here
