(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)

(defun my/ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c++-mode-hook 'my/ac-c-headers-init)
(add-hook 'c-mode-hook 'my/ac-c-headers-init)

(defun my/ac-add-semantic ()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my/ac-add-semantic)




(provide 'setup-autocomplete)
