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

;; Paredit is the best package ever.
(use-package paredit
  :ensure t
  :diminish "P"
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)
  ;; First remove some unused keys
  (define-key paredit-mode-map (kbd "M-;") nil)
  (define-key paredit-mode-map (kbd "C-<left>") nil)
  (define-key paredit-mode-map (kbd "C-<right>") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)

  ;; Try to make it play well with hungry delete
  ;; forward delete
  (defvar ach-whitespace-eol-re "\\(\\s-\\|$\\)+")
  (defvar ach-whitespace-bol-re "\\(\\s-\\|^\\)+")
  (defun paredit-forward-hungry-delete (orig-fun &rest argz)
    (if (not (looking-at ach-whitespace-eol-re))
        (apply orig-fun argz)
      (unless (car argz)
        (setq argz '(1)))
      (apply 'hungry-delete-forward argz)))

  (advice-add 'paredit-forward-delete :around #'paredit-forward-hungry-delete)

  (defun paredit-backward-hungry-delete (orig-fun &rest argz)
    (if (not (looking-back ach-whitespace-bol-re))
        (apply orig-fun argz)
      (unless (car argz)
        (setq argz '(1)))
      (apply 'hungry-delete-backward argz)))

  (advice-add 'paredit-backward-delete :around #'paredit-backward-hungry-delete)
  )

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
