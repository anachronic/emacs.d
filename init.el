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

;; custom wtvr
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-semantic company-elisp company-nxml company-css company-eclim company-xcode company-cmake company-capf company-yasnippet
		  (company-dabbrev-code company-gtags company-etags company-keywords)
		  company-oddmuse company-files)))
 '(custom-safe-themes
   (quote
    ("79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(erc-nick "anachronic")
 '(linum-relative-current-symbol "")
 '(pdf-view-display-size (quote fit-height))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "<f5>")
 '(shell-pop-window-position "right")
 '(shell-pop-window-size 50))

;; todos los .el custom
(add-to-list 'load-path "~/.emacs.d/elcustom/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; redireccionar autosave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; require pkgs
(require 'setup-lines)
(require 'expand-region)
(require 'helm-config)
(require 'helm-custom)
(require 'autopair)
(require 'setup-helm-gtags)
(require 'setup-markdown-mode)
;; (require 'highlight-lines)
(require 'shell-pop)
(require 'setup-c-compilation)
(require 'setup-python)
(require 'linum-relative)

;;;;;;;;;;;;;;;;;;;;;; configs
(powerline-center-theme)
;(load-theme 'seti t)
(load-theme 'atom-one-dark t)
(yas-global-mode 1)
(setq inhibit-startup-message t)
(autopair-global-mode)
(yas-reload-all)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
(show-paren-mode 1)
(pdf-tools-install)
(setq gdb-many-windows t)
(pending-delete-mode 1)
(setq scroll-step 1) ; Just use Ctrl-L to readjust the screen.
(global-hl-line-mode)


;; projectile+helm+neotree
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq projectile-completion-system 'helm)
(setq projectile-keymap-prefix (kbd "M-p"))
(projectile-global-mode)
(helm-projectile-on)
(smex-initialize)

;; linum-relative
(global-linum-mode)


;; Using AUCTeX with Evince.
(require 'evince-synctex)

;; Recent Files.
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-S-x C-S-f") 'helm-recentf)


;; requires que tienen dependencias arriba
(require 'setup-helm-gtags)
(require 'setup-semantic)
(require 'setup-company)

;; Make man pages wrap the buffer so it's readable in potentially small windows
(defun my/wrap-the-buffer ()
  (visual-line-mode))

(add-hook 'Man-mode-hook 'my/wrap-the-buffer)

;;;;;;;;;;;;;;;;;;;;;;;; END config


;; visual.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; rebind keys
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)  ;; Legacy
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)
(global-set-key (kbd "<f9>") 'neotree-toggle)
(global-set-key (kbd "<f10>") 'ff-find-other-file)
(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-S-<SPC>") 'company-complete) ;; C-SPC is classic, but its bound to mark, which i actually use
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-S-d") 'kill-whole-line)

(global-unset-key (kbd "C-z")) ; suspend was the most annoying thing ever
(global-set-key (kbd "C-z") 'undo)



(setq-default cursor-type 'bar)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
(provide 'init)
;;; init.el ends here
