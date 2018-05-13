;;; setup-evil.el --- Evil mode.
;;; Commentary:
;;; Code:

;; All evil variables that are going to be called by evil should go
;; here since evil already is loaded when the evil-leader use-package
;; sexp finishes interpreting
(setq-default evil-want-C-u-scroll t)
(setq-default evil-want-C-d-scroll t)
(setq-default evil-search-module 'evil-search)

(use-package evil-leader
  :demand t
  :ensure t
  :commands (global-evil-leader-mode))

(use-package evil
  :commands (evil-mode evil-define-key)
  :ensure t
  :demand t
  :init
  (global-evil-leader-mode)
  :config
  (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode"))
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'ivy-switch-buffer

    ;; buffers
    "k" 'kill-this-buffer
    "w" 'save-buffer
    "br" 'rename-buffer

    ;; rg
    "a" 'counsel-projectile-rg

    ;; Dired
    "gj" 'dired-jump

    ;; projectile
    "pp" 'projectile-switch-project
    )
  (evil-mode 1))

(provide 'setup-evil)
;;; setup-evil.el ends here
