;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; todos los .el custom
(add-to-list 'load-path "~/.emacs.d/elcustom/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; redireccionar autosave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; require pkgs
(require 'movelines)
(require 'powerline)
(require 'helm-config)
(require 'helm-custom)
(require 'autopair)

;; configs
(ac-config-default)
(powerline-center-theme)
(load-theme 'seti t)
(yas-global-mode 1)
(setq inhibit-startup-message t)
(autopair-global-mode)
(yas-reload-all)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
(show-paren-mode 1)

;; projectile+helm
(setq projectile-completion-system 'helm)
(setq projectile-keymap-prefix (kbd "M-p"))
(projectile-global-mode)
(helm-projectile-on)


;; powerline
(set-face-attribute 'mode-line nil
		    :foreground "Gray"
		    :background "#492b94"
		    :box nil)

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
