;;; setup-emacs.el --- Emacs related stuff.
;;; Commentary:
;;; Code:

;; Place here everything that is either a library, or sensible emacs defaults

;; This is useful for config.
(use-package restart-emacs
  :ensure t)

;; s.el is useful for our own functions
(use-package s
  :ensure t)

;; we need the filter function!!
(use-package dash
  :ensure t)

;; I've been using f.el lately
(use-package f
  :ensure t)

;; Misc crap that i don't know where to put
(use-package epc
  :ensure t)

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

;; might as well explicitly tell emacs we don't like tabs
(setq-default indent-tabs-mode nil)

;; From Mathieu Marques (@angrybacon at github)
;; This should make it a little less sluggish
(add-hook 'focus-out-hook #'garbage-collect)

;; I ran into a VERY weird issue where C-n is the slowest thing
;; ever. Fortunately, I found an answer
;; https://lists.gnu.org/archive/html/emacs-devel/2006-09/msg00634.html
;; Actually this, for some reason, makes the C-n be very fast even in
;; very large buffers. Weird
(setq auto-window-vscroll nil)

;; no DING!
(setq visible-bell 1)

;; Enable narrow commands
(put 'narrow-to-region 'disabled nil)

(provide 'setup-emacs)
;;; setup-emacs.el ends here
