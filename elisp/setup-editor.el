(show-paren-mode 1)
(global-hl-line-mode)

;; From Howard Abrams .emacs
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "C-<f7>") (lambda () (interactive) (other-window -1)))

(provide 'setup-editor)
