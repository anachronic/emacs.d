;;; latex-writing.el --- LaTeX setup.
;;; Commentary:
;;; Code:

;; AUCTeX: This is critical for me and LaTeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode))

;; We'd also like to instantly preview our TeX docs.
;; It's quite nice that latex-preview-pane works well with DocView
(use-package latex-preview-pane
  :ensure t
  :after auctex
  :diminish "Preview"
  :config
  (latex-preview-pane-enable))

;; The following doesn't play nice with use-package. Whatever.
(defun my/add-preview-pane-toggle ()
  "Add latex preview pane keybinding."
  (local-set-key (kbd "C-c M P") 'latex-preview-pane-mode))
(add-hook 'LaTeX-mode-hook 'my/add-preview-pane-toggle)

;; Let's try Flyspell with TeX documents.
(defun my/add-flyspell ()
  "Add Flyspell to a buffer and diminish it to FlyS."
  (flyspell-mode)
  (diminish 'flyspell-mode "FlyS"))
(add-hook 'LaTeX-mode-hook #'my/add-flyspell)

;; Enable narrowing. God disabled commands are annoying.
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Also, autosave in LaTeX mode
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(provide 'latex-writing)
;;; latex-writing.el ends here
