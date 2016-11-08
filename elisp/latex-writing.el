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
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Also diminish Flyspell to FlyS
(diminish 'flyspell-mode "FlyS")

;; Enable narrowing. God disabled commands are annoying.
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'latex-writing)
;;; latex-writing.el ends here
