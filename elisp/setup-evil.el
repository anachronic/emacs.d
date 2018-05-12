;;; setup-evil.el --- Evil mode.
;;; Commentary:
;;; Code:

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
  (evil-define-key 'normal global-map (kbd "C-p") 'counsel-projectile-find-file)
  (evil-leader/set-key
    "<SPC>" 'ivy-switch-buffer

    ;; buffers
    "k" 'kill-this-buffer
    "w" 'save-buffer
    "br" 'rename-buffer

    ;; rg
    "a" 'counsel-projectile-rg

    ;; projectile
    "pp" 'projectile-switch-project
    )
  (evil-mode 1))

(provide 'setup-evil)
;;; setup-evil.el ends here
