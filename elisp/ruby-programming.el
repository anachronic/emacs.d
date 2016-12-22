;;; ruby-programming.el --- Ruby related stuff.
;;; Commentary:
;;; Code:

;; The first thing is to automatically add end to any starting
;; sequence like def, if, while, do, etc.
(use-package ruby-end
  :ensure t
  :config
  (setq ruby-end-insert-newline nil))

;; Ruby is one of those annoying 2-space indent languages. Let's use a
;; guide
(add-hook 'ruby-mode-hook #'indent-guide-mode)

;; yari. Docs are cool. Need to pacman -S ruby-docs for it to work.
(use-package yari
  :ensure t)

(provide 'ruby-programming)
;;; ruby-programming.el ends here
