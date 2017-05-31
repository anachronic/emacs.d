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

;; ivy-hydra is not too horrible
(use-package ivy-hydra
  :ensure t
  :demand)

;; I've come to think helm is not really good with files anymore. So let's
;; use counsel. It also has some nice builtin functionality:
;; http://oremacs.com/2015/04/19/git-grep-ivy/
(use-package counsel
  :ensure t
  :after (ivy flx smex)
  :demand
  :bind (("C-." . counsel-imenu)
         ("M-x" . counsel-M-x)
         ("C-x f" . counsel-recentf))
  :config
  ;; Need these key bindings for terminals
  (define-key meta-m-map (kbd "M-r") #'counsel-rhythmbox))

;; I had a mix of stuff before. So it's nice to decide on
;; counsel-projectile after all. I don't even projectile that much,
;; but using counsel makes it way more consistent than it was before
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :bind (("C-c a" . counsel-projectile-ag)
         ("C-S-p" . counsel-projectile-find-file))
  :config
  (defun nsv/switch-buffer-maybe-on-project ()
    "Call counsel-projectile-switch-to-buffer if on project, ivy-switch-buffer otherwise"
    (interactive)
    (if (projectile-project-p)
        (call-interactively 'counsel-projectile-switch-to-buffer)
      (call-interactively 'ivy-switch-buffer)))
  (global-set-key (kbd "C-S-m") #'nsv/switch-buffer-maybe-on-project)
  (define-key meta-m-map (kbd "M-m") #'nsv/switch-buffer-maybe-on-project)
  (define-key meta-m-map (kbd "M-p") #'counsel-projectile-find-file)
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
  (define-key company-active-map (kbd "C-j") 'company-complete-common-or-cycle)

  ;; default backends
  (setq-default company-backends
                '(company-bbdb
                  company-nxml company-css
                  company-eclim company-semantic company-clang
                  company-xcode company-cmake
                  company-capf
                  company-files
                  (company-dabbrev-code company-gtags company-etags
                                        company-keywords)
                  company-oddmuse))
  :diminish "comp"
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
  (setq helm-buffers-fuzzy-matching t)
  (with-eval-after-load 'counsel
    (define-key helm-command-prefix (kbd "c") #'counsel-colors-emacs)))

;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-/") 'hippie-expand)

;; I saw this talk and it blew my mind about some vim completions.
;; https://youtu.be/3TX3kV3TICU
;; Hippie expand can do something similar
;; Source: https://stackoverflow.com/a/17928654/2066658
(defun expand-line ()
  "Expand current line using `hippie-expand'."
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))

(define-key ctl-x-map (kbd "C-l") 'expand-line)

(provide 'setup-completions)
;;; setup-completions.el ends here
