;;; setup-vc.el --- Version control related config.
;;; Commentary:
;;; Code:

(require 'f)

(use-package evil-magit
  :ensure t
  :defer t)

;; Magit is critical for any developer
(use-package magit
  :ensure t
  :init
  ;; magit-status is autoloaded
  (evil-leader/set-key
    "gg" 'magit-status)
  :bind (("<f8>" . magit-status)
         ("C-x g" . magit-status))
  :config
  (setq magit-repository-directories '("~/forks"))
  (add-hook 'magit-mode-hook (lambda () (require 'evil-magit)))

  ;; We want to start typing right away when committing
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  ;; Need for pushing
  (add-hook 'magit-mode-hook (lambda ()
                               (exec-path-from-shell-copy-env "SSH_AGENT_PID")
                               (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")))
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil)
  )

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
                                 " -")))
  (defun projectile-magit-action ()
    "Switch to magit status action when invoking projectile."
    (magit-status-internal (projectile-project-root)))
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
  (add-hook 'prog-mode-hook #'turn-on-diff-hl-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'setup-vc)
;;; setup-vc.el ends here
