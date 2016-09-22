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
  (helm-mode 1)
  (require 'helm-config)
  (setq helm-mode-fuzzy-match t)
  :bind (("C-x C-f" . helm-find-files))
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action))

;; Visual configuration. Here you'll find stuff about how Emacs looks
(require 'setup-ui)

;; Recentf is small enough. We set it up right here.
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-S-x C-S-f") 'helm-recentf)

;; Magit is critical for any developer
(use-package magit
  :bind ("<f8>" . magit-status))













(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
