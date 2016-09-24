;; load the theme
(load-theme 'caroline t)

(require 'hl-line)
(set-face-background hl-line-face "#404B4F")

(show-paren-mode 1)
(global-hl-line-mode)

;; From Howard Abrams .emacs
;; F7 was too centerish. F12 should be good
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-<f12>") (lambda () (interactive) (other-window -1)))

(provide 'setup-editor)
;;; setup-editor.el ends here
