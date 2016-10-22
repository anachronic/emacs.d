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
    (setq web-mode-comment-style 2)
    (setq web-mode-script-padding 0)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-engines-alist
          '(("django"    . "\\.html\\'"))
          )))

(defun my/web-mode-before-function-p (id action context)
  "Check if the char 2 positions before point is a closing paren."
  (and (eq action 'insert)
       (equal (progn (save-excursion
                       (backward-char)
                       (string (preceding-char))))
              ")")))

(sp-local-pair 'web-mode "{" "}" :when '(my/web-mode-before-function-p))

;; Emmet!!!
(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)))

(provide 'web-programming)
;;; web-programming.el ends here
