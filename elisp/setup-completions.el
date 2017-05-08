;;; setup-ivy-helm-completions.el --- General completion system.
;;; Commentary:
;;; Code:

;;; This file handles every completion system my config has. That
;;; means: minibuffer completions, file search, grep, autocompletion
;;; when coding and snippets. It also includes every package related
;;; to them, like company enhancements, for instance.

;; Smex provides a GREAT interface for M-x. I got rid of it at some
;; point but i really regret it, mostly for the last used commands.
(use-package smex
  :ensure t)

;; I've really gotten into Ivy. I think I'll be dropping helm
;; soon. Especially because I managed to make ivy behave the way I
;; wanted with tab and C-l
(use-package ivy
  :ensure t
  :demand
  :diminish 'ivy-mode
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-git-grep . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist
        '((counsel-M-x . "^")
          (man . "^")
          (woman . "^")))
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 16)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done)
  (define-key ivy-minibuffer-map [tab] #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-backward-delete-char)
  (ivy-mode 1))

;; I've come to think helm is not really good with files anymore. So let's
;; use counsel. It also has some nice builtin functionality:
;; http://oremacs.com/2015/04/19/git-grep-ivy/
(use-package counsel
  :ensure t
  :after (ivy flx smex)
  :demand
  :bind (("C-." . counsel-imenu)
         ("C-c g" . counsel-git-grep)
         ("C-S-p" . counsel-git)
         ("M-x" . counsel-M-x))
  :config
  ;; Need these key bindings for terminals
  (define-key meta-m-map (kbd "M-p") #'counsel-git)
  )

;; I had a mix of stuff before. So it's nice to decide on
;; counsel-projectile after all. I don't even projectile that much,
;; but using counsel makes it way more consistent than it was before
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :bind (("C-c a" . counsel-projectile-ag))
  :config
  (defun nsv/switch-buffer-maybe-on-project ()
    "Call counsel-projectile-switch-to-buffer if on project, ivy-switch-buffer otherwise"
    (interactive)
    (if (projectile-project-p)
        (call-interactively 'counsel-projectile-switch-to-buffer)
      (call-interactively 'ivy-switch-buffer)))
  (global-set-key (kbd "C-S-m") #'nsv/switch-buffer-maybe-on-project)
  (define-key meta-m-map (kbd "M-m") #'nsv/switch-buffer-maybe-on-project)
  (counsel-projectile-on))

;; I guess this can't hurt
(use-package ivy-rich
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer 'ivy-rich-switch-buffer-transformer))

;; YASnippet, always so handy...
(use-package yasnippet
  :ensure t
  :diminish 'yas-minor-mode
  :config
  (setq-default yas-snippet-dirs `(,(expand-file-name
                                     (concat user-emacs-directory
                                             "snippets"))))
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-<return>") 'yas-exit-snippet)
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

;; I do like an autocompletion system, even though this really has to
;; mature in Emacs.
(use-package company
  :ensure t
  :after yasnippet
  :demand
  :config
  (defun nsv/company-complete-if-only-one ()
    "Complete candidate if there's only one option, otherwise yas-expand"
    (interactive)
    (if (eq 1 company-candidates-length)
        (company-complete)
      (yas-expand)))
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'comint-mode-hook #'company-mode)
  (setq company-idle-delay 0.5)
  (define-key company-active-map (kbd "TAB") #'nsv/company-complete-if-only-one)
  (define-key company-active-map (kbd "<tab>") #'nsv/company-complete-if-only-one)
  :diminish "comp"  ;; I'm really hating company in non-programming modes
  :bind (("C-S-<SPC>" . company-complete)))

;; Help is cool
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;; Company statistics. I do use this a lot, maybe i should't be coding
;; like I do...
(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode))

;; flx is a dependency for company-flx and ivy's M-x (or any ivy
;; actually).
(use-package flx
  :demand
  :ensure t)

;; Will be trying company-flx for a while.
(use-package company-flx
  :ensure t
  :after flx
  :config
  (company-flx-mode +1)
  (setq company-flx-limit 75))


;; Now that ivy is set up, let's have helm. I'd rather not have it,
;; but it has grown in such a way that many packages use helm for
;; useful stuff. Seriously, I've seen from controlling spotify to
;; chrome. It seems like waste not to have that.

;; The only one thing i really miss about helm when using ivy is
;; helm-mini. I mean, to have recentf is such a cool feature you don't
;; have to set any weird keybindings for anything anymore.
(use-package helm
  :ensure t
  :demand
  :bind (("M-y" . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (global-set-key [remap occur] #'helm-occur)
  (setq helm-buffers-fuzzy-matching t))


(provide 'setup-completions)
;;; setup-completions.el ends here
