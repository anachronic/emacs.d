;;; js-programming.el --- Javascript programming relevant stuff.
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :commands (js2-mode js2-jsx-mode)
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2)
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-idle-timer-delay 0.4)
  (add-hook 'js2-mode-hook (lambda () (color-identifiers-mode -1)))
  (with-eval-after-load 'dumb-jump
    (define-key js2-mode-map (kbd "M-.") 'dumb-jump-go)
    (define-key js2-mode-map (kbd "M-,") 'dumb-jump-back))

  ;; Set sensible mode lighters...
  (defun ach--set-js2-mode-lighter ()
    "Change `js2-mode' lighter to Javascript"
    (setq-local mode-name "Javascript"))
  (add-hook 'js2-mode-hook 'ach--set-js2-mode-lighter)

  (defun ach--set-js2-jsx-mode-lighter ()
    "Change `js2-jsx-mode' lighter to JSX"
    (setq-local mode-name "JSX"))
  (add-hook 'js2-jsx-mode-hook 'ach--set-js2-jsx-mode-lighter)
  )

;; Tern is good like, for completion
(use-package tern
  :ensure t
  :defer t
  :diminish ""
  :init
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  )

;; See above
(use-package company-tern
  :ensure t
  :after (company tern js2-mode)
  :defer t
  :config
  (defun ach-add-tern-company ()
    (setq-local company-backends company-backends)
    (add-to-list 'company-backends 'company-tern))
  (add-hook 'js2-mode-hook #'ach-add-tern-company))

;; Tends to be a little crappy when finding definitions but is quite
;; useful for references. We use the default key: M-?
(when (executable-find "ag")
  (use-package xref-js2
    :ensure t
    :defer t
    :init
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))

;; Been using this one for work lately
(use-package json-reformat
  :ensure t)

;; js-comint. I have the need for this now
(use-package js-comint
  :ensure t
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c C-s") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c C-z") 'run-js))))

(provide 'js-programming)
;;; js-programming.el ends here
