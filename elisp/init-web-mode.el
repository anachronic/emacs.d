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
  (setq web-mode-comment-style 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'"))
        )

  (defvar web-mode-inhibit-control-block-strings
    '("stylesheet" "endstylesheet" "javascript" "endjavascript"))
  (dolist (str web-mode-inhibit-control-block-strings)
    (delete str web-mode-django-control-blocks)))

;; This function should return t when chr is {, mode is web-mode
;; and the next character _before_ point (without spaces and/or newlines)
;; is __NOT__ a closing paren
(defun ach-web-mode-before-function-p (chr)
  "True if CHR is {, mode is web-mode and previous character is NOT a closing paren."
  (when (eq ?\{ chr)
    (not (looking-back ")\\s-*{"))))

(require 'elec-pair)
(add-hook 'web-mode-hook
          (lambda () (setq-local electric-pair-inhibit-predicate
                            'ach-web-mode-before-function-p)))

;; Single quotes don't pair in web mode
(defun ach-web-mode-single-quote-pair ()
  "Define single quote pair in web mode."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs '((39 . 39)))))
(add-hook 'web-mode-hook 'ach-web-mode-single-quote-pair)

;; A great package. Not only does it complete HTML stuff, but it also
;; displays documentation with company-quickhelp
(use-package company-web
  :ensure t)

;; This package works wonders. I have to say I didn't expect it to.
(use-package ac-html-bootstrap
  :ensure t)

(defun ach-add-company-backends-webmode ()
  "Add Tern, webmode and bootstrap backends to company."
  (setq-local company-backends company-backends)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-tern)
  (company-web-bootstrap+))

(add-hook 'web-mode-hook 'ach-add-company-backends-webmode)

;; Emmet!!!
(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)))

;; get rid of color identifiers
(add-hook 'web-mode-hook (lambda () (color-identifiers-mode -1)))

(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "M-.") #'dumb-jump-go)
  (define-key web-mode-map (kbd "M-,") #'dumb-jump-back))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
