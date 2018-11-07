;;; markdown-writing.el --- Markdown writing configuration.
;;; Commentary:
;;; Code:

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-style "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'gfm-mode-hook #'auto-fill-mode)
(add-hook 'gfm-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

(provide 'init-markdown)
;;; init-markdown.el ends here
