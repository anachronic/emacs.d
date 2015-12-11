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

;; require pkgs
(require 'movelines)
(require 'powerline)
(require 'helm-config)
(require 'helm-custom)

;; configs
(ac-config-default)
(powerline-center-theme)
(load-theme 'blackboard t)


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
