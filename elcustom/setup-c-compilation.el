
(defun my/rebind-compile-c-fn ()
  (interactive)
  (setq-local compilation-read-command nil)
  (set (make-local-variable 'compile-command) "make -k debug")
  (call-interactively 'compile))

(defun my/c-bind-compile ()
  (global-set-key (kbd "<f6>") 'my/rebind-compile-c-fn))

(add-hook 'c-mode-hook 'my/c-bind-compile)
(add-hook 'c++-mode-hook 'my/c-bind-compile)


(provide 'setup-c-compilation)
