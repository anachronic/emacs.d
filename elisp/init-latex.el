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
  :diminish "P"
  :config
  (latex-preview-pane-enable))

;; The following doesn't play nice with use-package. Whatever.
(defun ach-add-preview-pane-toggle ()
  "Add latex preview pane keybinding."
  (local-set-key (kbd "C-x p") 'latex-preview-pane-mode))
(add-hook 'LaTeX-mode-hook 'ach-add-preview-pane-toggle)

;; Let's try Flyspell with TeX documents.
(defun ach-add-flyspell ()
  "Add Flyspell to a buffer and diminish it to FlyS."
  (flyspell-mode)
  (diminish 'flyspell-mode "S"))
(add-hook 'LaTeX-mode-hook #'ach-add-flyspell)

;; Enable narrowing. God disabled commands are annoying.
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Also, autosave in LaTeX mode
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

;; Been needing company-bibtex lately
(use-package company-bibtex
  :ensure t
  :config
  (defun ach-load-company-bibtex ()
    "Add bibtex backend to company."
    (company-mode 1)
    (require 'company-bibtex)
    (setq-local company-backends company-backends)
    (setq company-bibtex-bibliography "~/Dropbox/tesis/referencias.bib")
    (add-to-list 'company-backends 'company-bibtex))
  (add-hook 'LaTeX-mode-hook #'ach-load-company-bibtex))


(provide 'init-latex)
;;; init-latex.el ends here
