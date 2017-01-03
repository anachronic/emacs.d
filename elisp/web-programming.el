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
    (setq web-mode-comment-style 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-engines-alist
          '(("django"    . "\\.html\\'"))
          )))

;; This function should return t when chr is {, mode is web-mode
;; and the next character _before_ point (without spaces and/or newlines)
;; is __NOT__ a closing paren
(defun my/web-mode-before-function-p (chr)
  "True if CHR is {, mode is web-mode and previous character is NOT a closing paren."
  (if (not (eq major-mode 'web-mode))
      nil
    (and (eq major-mode 'web-mode)
         (eq ?\{ chr)
         (not (equal (progn (save-excursion
                              (backward-char)
                              (string (preceding-char))))
                     ")")))))

(setq electric-pair-inhibit-predicate
      'my/web-mode-before-function-p)

;; A great package. Not only does it complete HTML stuff, but it also
;; displays documentation with company-quickhelp
(use-package company-web
  :ensure t)

;; This package works wonders. I have to say I didn't expect it to.
(use-package ac-html-bootstrap
  :ensure t)

(defun my/add-company-backends-webmode ()
  "Add Tern, webmode and bootstrap backends to company."
  (setq-local company-backends company-backends)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-tern)
  (company-web-bootstrap+))

(add-hook 'web-mode-hook 'my/add-company-backends-webmode)

(defun my/add-keychords-web-mode ()
  "Add ;; keystroke to mean insert semicolon at the end of the line."
  (key-chord-define-local ";;" "\C-e;"))

(add-hook 'web-mode-hook 'my/add-keychords-web-mode)

;; Emmet!!!
(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)))

;; The tabs really get in the way sometimes.
(add-hook 'web-mode-hook #'indent-guide-mode)

(provide 'web-programming)
;;; web-programming.el ends here
