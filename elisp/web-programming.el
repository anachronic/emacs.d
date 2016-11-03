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

(defun my/web-mode-before-function-p (chr)
  "Return false if CHR is {, major mode is web-mode and previous char is closing paren."
  (if (not (eq major-mode 'web-mode))
      nil
    (not (and (eq major-mode 'web-mode)
              (eq ?\{ chr)
              (equal (progn (save-excursion
                              (backward-char)
                              (string (preceding-char))))
                     ")")))))

;; A great package. Not only does it complete HTML stuff, but it also
;; displays documentation with company-quickhelp
(use-package company-web
  :ensure t)

;; This package works wonders. I have to say I didn't expect it to.
(use-package ac-html-bootstrap
  :ensure t)

(defun my/add-company-backends-webmode ()
  "Add Tern, webmode and bootstrap backends to company."
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-tern)
  (company-web-bootstrap+))

(add-hook 'web-mode-hook 'my/add-company-backends-webmode)


(setq electric-pair-inhibit-predicate
      'my/web-mode-before-function-p)

;; Emmet!!!
(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)))

(provide 'web-programming)
;;; web-programming.el ends here
