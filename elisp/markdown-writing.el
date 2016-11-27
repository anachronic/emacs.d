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
  :ensure t)

(defun my/add-markdown-preview-key ()
  "Set a new key for markdown-preview-mode."
  (with-eval-after-load 'markdown-mode
    (with-eval-after-load 'markdown-preview-mode
      (local-set-key (kbd "C-c C-c C-p") #'markdown-preview-mode)
      (local-set-key (kbd "C-c C-c C-o") #'markdown-preview-open-browser))))

(add-hook 'markdown-mode-hook #'my/add-markdown-preview-key)

(provide 'markdown-writing)
;;; markdown-writing.el ends here
