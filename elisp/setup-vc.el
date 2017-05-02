;;; setup-vc.el --- Version control related config.
;;; Commentary:
;;; Code:

;; Magit is critical for any developer
(use-package magit
  :ensure t
  :bind (("<f8>" . magit-status)
         ("C-x g" . magit-status))
  :config
  (setq magit-repository-directories '("~/forks")))

;; This one can't really be up there, because that doesn't load
;; anything until you actually fire up Magit.
(define-key vc-prefix-map (kbd "h") #'magit-log-buffer-file)

;; This is not really version control, but it fits into the category
(use-package projectile
  :ensure t
  :demand
  :config
  (projectile-mode)
  (setq-default projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line '(:eval
                               (when (not (projectile-project-p))
                                 " [-]")))
  (defun projectile-magit-action ()
    "Switch to magit status action when invoking projectile."
    (magit-status (projectile-project-root)))
  (setq projectile-switch-project-action 'projectile-magit-action)
  (global-set-key (kbd "C-S-o") 'projectile-switch-project))

;; Git ignore modes, and misc stuff major modes.
(use-package gitignore-mode
  :ensure t)

;; browse-at-remote. I do github browsing a lot. So let's use this!
(use-package browse-at-remote
  :ensure t)

;; gist. I'm sure I'll use this someday.
(use-package gist
  :ensure t)

;; diff-hl. I kind of liked the IntelliJ approach to this
(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'turn-on-diff-hl-mode))

(provide 'setup-vc)
;;; setup-vc.el ends here
