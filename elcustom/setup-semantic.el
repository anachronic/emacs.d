(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(require 'stickyfunc-enhance)
(defun my/c-stickyfunc ()
  (global-semantic-stickyfunc-mode))

(add-hook 'c-mode-hook 'my/c-stickyfunc)
(add-hook 'c++-mode-hook 'my/c-stickyfunc)

(provide 'setup-semantic)
