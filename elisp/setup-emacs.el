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

;; s.el is useful for our own functions
(use-package s
  :ensure t)

;; we need the filter function!!
(use-package dash
  :ensure t)

;; I've been using f.el lately
(use-package f
  :ensure t)

;; Misc crap that i don't know where to put
(use-package epc
  :ensure t)

(provide 'setup-emacs)
;;; setup-emacs.el ends here
