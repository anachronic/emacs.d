(require 'hl-line)
(set-face-background hl-line-face "#404B4F")

(show-paren-mode 1)
(global-hl-line-mode)

;; Make C-n add newlines at end of file. Should test this.
(setq next-line-add-newlines t)

;; Linum relative for good editing
(use-package nlinum-relative
  :ensure t
  :config
  ;; something else you want
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-redisplay-delay 0))

;; I want to be able to comment stuff easily :)

;; From Howard Abrams .emacs
;; F7 was too centerish. F12 should be good
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-<f12>") (lambda () (interactive) (other-window -1)))

;; I'm a little bit too young for emacs,
;; so I like replacing the region with stuff.
(delete-selection-mode 1)


;; Wrapping the buffer is very useful in org mode and latex mode
(defun my/visualmode ()
  (visual-line-mode))

(add-hook 'org-mode-hook 'my/visualmode)
(add-hook 'Man-mode-hook 'my/visualmode)

;; Also, autosave in LaTeX mode
(add-hook 'LaTeX-mode-hook 'my/visualmode)
(setq TeX-auto-save t)


;; diminish visual-line-mode
(diminish 'visual-line-mode)


(provide 'setup-editor)
;;; setup-editor.el ends here
