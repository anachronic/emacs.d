;;; setup-emacs.el --- Emacs related stuff.
;;; Commentary:
;;; Code:

;; Place here everything that is either a library, or sensible emacs defaults

;; This is useful for config.
(use-package restart-emacs
  :ensure t)

;; libs
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package f :ensure t)
(use-package epc :ensure t)
(use-package diminish :ensure t)

(diminish 'eldoc-mode)

;; Allow connecting from a terminal or whatever.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Other generally useful functions
(defun maybe-load-file (file)
  "Load FILE only if it exists."
  (when (file-exists-p file)
    (load-file file)))

;; get rid of everything other than the title at the top.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

;; also get rid of the scratch buffer message
(setq initial-scratch-message nil)


;; From Mathieu Marques (@angrybacon at github)
;; This should make it a little less sluggish
(add-hook 'focus-out-hook #'garbage-collect)

;; no DING!
(setq visible-bell 1)

;; Enable narrow commands
(put 'narrow-to-region 'disabled nil)

;; From purcell's config
(global-auto-revert-mode)
(setq-default global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)

(provide 'setup-emacs)
;;; setup-emacs.el ends here
