;;; setup-emacs.el --- Emacs related stuff.
;;; Commentary:
;;; Code:

;; It's not quite clear what should be here, so here I go: Anything
;; that can classify as internal emacs configuration or any library
;; required by other packages. Examples include garbage collection,
;; changing yes/no for y/n, installing dash.el, s.el and whatever you
;; can think of

;; This is useful for config.
(use-package restart-emacs
  :ensure t)

;;; Set garbage collection back to a normal value
;; I hope it doesn't make it hang again..
(setq gc-cons-threshold 128000000)

;; s.el is useful for our own functions
(use-package s
  :ensure t)


(provide 'setup-emacs)
;;; setup-emacs.el ends here
