;;; markdown-writing.el --- Markdown writing configuration.
;;; Commentary:
;;; Code:

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-style "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

(defun nsv/add-markdown-preview-key ()
  "Set a new key for markdown-preview-mode."
  (with-eval-after-load 'markdown-mode
    (with-eval-after-load 'markdown-preview-mode
      (local-set-key (kbd "C-c C-c C-p") #'markdown-preview-mode)
      (local-set-key (kbd "C-c C-c C-o") #'markdown-preview-open-browser))))

(add-hook 'markdown-mode-hook #'nsv/add-markdown-preview-key)
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'gfm-mode-hook #'auto-fill-mode)
(add-hook 'gfm-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

(provide 'markdown-writing)
;;; markdown-writing.el ends here
