;;; elisp-programming.el --- Emacs lisp programming related code.
;;; Commentary:
;;; Code:

;; Got this from purcell's. I like pretty buffers, you know?
(use-package page-break-lines
  :ensure t
  :diminish ""
  :config
  (global-page-break-lines-mode))

;; cask mode
(use-package cask-mode
  :ensure t)

;; I like prettify symbols mode. but only for elisp
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

;; Replacing paredit in evil is quite a pain
;; (use-package evil-cleverparens
;;   :ensure t
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode))

(use-package lispyville
  :ensure t
  :diminish "L"
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     text-objects
     prettify
     additional-motions
     slurp/barf-lispy
     wrap
     additional
     additional-insert)))

;; Stealing conf from purcell's .emacs.d. This package is actually
;; pretty cool, it gets you out of the dullness of full white text (or
;; should I say: default face)
(use-package highlight-quoted
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; Eval buffer is nice.
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer)

;; From Endless Parentheses
;; http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html

;; Someone decided to implement it, which is cool, since CIDER is
;; quite a heavy package
(use-package eros
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

(provide 'init-elisp)
;;; init-elisp.el ends here
