;;; setup-ediff.el --- Ediff config.
;;; Commentary:
;;; Code:

;; I think the config from abo-abo should be good
;; http://oremacs.com/2015/01/17/setting-up-ediff/

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)


(provide 'setup-ediff)
;;; setup-ediff.el ends here
