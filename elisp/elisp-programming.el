;;; elisp-programming.el --- Emacs lisp programming related code.
;;; Commentary:
;;; Code:

;; Got this from purcell's. I like pretty buffers, you know?
(use-package page-break-lines
  :ensure t
  :diminish ""
  :config
  (global-page-break-lines-mode))


;; I like prettify symbols mode. but only for elisp
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

;; I'll be trying Paredit. Should be quite useful for lisp-like stuff
;; Yeah, I've been becoming more fond of paredit every day. It rocks!
;; However, I don't use C-j for anything, since electric indent does
;; everything for me, and if I need a key binding for newline, well, I
;; already have C-m.
(use-package paredit
  :ensure t
  :diminish "par"
  :config
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "M-;") nil)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

  ;; This thing is so freaking useful it freaks me out a little.
  (global-set-key (kbd "M-K") #'paredit-kill))

;; Stealing conf from purcell's .emacs.d. This package is actually
;; pretty cool, it gets you out of the dullness of full white text (or
;; should I say: default face)
(use-package highlight-quoted
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))


;; We continue the stealing: hl-sexp. Useful sometimes
(use-package hl-sexp
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(provide 'elisp-programming)
;;; elisp-programming.el ends here