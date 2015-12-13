(require 'auto-complete)
(require 'auto-complete-clang-async)
(require 'auto-complete-config)

(defun my/ac-config ()
  (setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-yasnippet))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(defun my/ac-c-hooks ()
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'ac-sources 'ac-source-clang))
(add-hook 'c-mode-hook 'my/ac-c-hooks)
(add-hook 'c++-mode-hook 'my/ac-c-hooks)

(my/ac-config)

(define-key ac-completing-map (kbd "RET") 'keyboard-quit)

(provide 'setup-autocomplete)
