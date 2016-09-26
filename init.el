;;; package --- Summary
;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; General paths. Self-written scripts and themes.
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Nobody likes those annoying ~ end files, so redirect them.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Base auto-install and pretty code for configuring packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Helm Config
;; Completion is HISTORICALLY bound to TAB on almost ANY editor. So we do that here.
(use-package helm
  :ensure t
  :init 
  (setq helm-mode-fuzzy-match t)
  :bind (("C-x C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1)
  (require 'helm-config)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action))

;; Recentf is small enough. We set it up right here.
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-S-x C-S-f") 'helm-recentf)

;; Magit is critical for any developer
(use-package magit
  :ensure t
  :bind ("<f8>" . magit-status))

;; Company: Not much customization right now.
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.3)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  :diminish "comp")

;; Some help can't hurt
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;; Company statistics. I do use this a lot, maybe i should't be coding like I do...
(use-package company-statistics
  :ensure t
  :commands company-statistics-mode)

;; Helm fuzzy mode doesn't seem to be as good as smex...
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

;; Autopair lets you code with no worries.
;; *especially* elisp, or any lisp based programming language
(use-package autopair
  :ensure t
  :init
  (autopair-global-mode)
  :diminish "")

;; YASnippet, always so handy...
(use-package yasnippet
  :ensure t
  :defer t
  :init
  (yas-global-mode 1))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Undo-Tree for real undo/redo commands
(use-package undo-tree
  :ensure t
  :diminish ""
  :init
  (global-undo-tree-mode 1)
  :bind
  (("C-z" . undo)
   ("C-S-z" . undo-tree-redo)))

;;;;; At this point I feel like im just copying people..
;;;;; thats fine though

;; ace-window from Howard Abrams. I hear its nice
(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "C-x o") 'ace-window))

;; Projectile, for projects
(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))
  (setq projectile-completion-system 'helm)
  (projectile-global-mode)
  :config
  (setq projectile-enable-caching t))


;; Flycheck. What's an editor without error checking?
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :diminish "Fchk")

;; AUCTeX: This is critical for me and LaTeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (setq TeX-auto-save t))

;; yaml-mode. mainly for syntax highlighting
(use-package yaml-mode
  :ensure t ;; seems like overkill
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;; expand region. An *excellent* tool.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; smart forward. I hear its nice.
;; Actually my current navigation tools kind of suck. Let's have a look
(use-package smart-forward
  :init
  (require 'expand-region)
  (global-set-key (kbd "M-<up>") 'smart-up)
  (global-set-key (kbd "M-<down>") 'smart-down)
  (global-set-key (kbd "M-<left>") 'smart-backward)
  (global-set-key (kbd "M-<right>") 'smart-forward)
  :ensure t)




;; load the latest theme.



;; We have all our modes set.
;; Simple config goes hardcoded here.
;; Grouped configuration files can be found in ./elisp

;; Add stuff to text so you get visual aid when coding
(require 'setup-editor)

;; Visual configuration. Here you'll find stuff about how Emacs looks
(require 'setup-ui)

;; Renaming an open buffer file.
(require 'renamefile)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (linum-relative auctex flycheck company-statistics helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
