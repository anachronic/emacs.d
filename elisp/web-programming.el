;;; web-programming.el --- Web programming configuration.
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :commands (web-mode)
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (progn
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-engines-alist
          '(("django"    . "\\.html\\'"))
          )))

(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)))

(provide 'web-programming)
;;; web-programming.el ends here
