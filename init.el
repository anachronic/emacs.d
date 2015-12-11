;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; todos los .el custom
(add-to-list 'load-path "~/.emacs.d/elcustom/")

;; moveline
(require 'movelines)

;; visual.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; rebind keys
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)  ;; Legacy
(global-set-key [C-tab] 'other-window)


