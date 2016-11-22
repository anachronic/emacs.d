;;; setup-ui.el --- UI related configuration.
;;; Commentary:
;;; Code:

;; get rid of everything other than the title at the top.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; these two come from: https://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

;; also get rid of the scratch buffer message
(setq initial-scratch-message nil)

;; load the latest theme.
;; (load-theme 'monokai t)
(load-theme 'avk-darkblue-white t)
(set-face-attribute 'default nil :height 105)

;;; Spaceline is the coolest modeline.
(use-package spaceline
  :ensure t
  :demand)

;; Unfortunately it does not display anything when the buffer is
;; narrowed. I'd like to have that, you know?
;; Let's do it. Define a segment that does exactly what I want.
(require 'spaceline-config)
(require 'spaceline)
(spaceline-define-segment narrow
  "Display Narrowed when buffer is narrowed."
  (when (buffer-narrowed-p)
    "Narrowed"))
(spaceline-spacemacs-theme 'narrow)
(setq spaceline-minor-modes-separator " ")

;; Frame commands
(global-set-key (kbd "C-c F") 'make-frame)

;; Switch frames.
(global-set-key (kbd "C-c o") 'other-frame)

;; emacs-close-dwim
;; I want to C-x C-c out of a frame without closing emacs.
;; So if there's more than one frame active. Just close it
;; If there's only one, shut down emacs. And hell, don't ask
;; to save buffers, just do it.
(defun my/emacs-close-dwim ()
  "Close current frame if there's more than one active.
Otherwise exit Emacs."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))

;; rebind it to C-x C-c
(define-key ctl-x-map (kbd "C-c") 'my/emacs-close-dwim)

;; use ibuffer instead of default C-x C-b
(define-key ctl-x-map (kbd "C-b") 'ibuffer)

;; no DING!
(setq visible-bell 1)

;; This has been driving me crazy. So new key binding.
(global-set-key (kbd "<f9>") 'split-window-right)

;; enlarge/shrink current window
(global-set-key (kbd "s-f") 'enlarge-window-horizontally)
(global-set-key (kbd "s-b") 'shrink-window-horizontally)

;; I like next/previous buffer handy. I don't always want to write
;; the name of the buffer i'm looking for.
(global-set-key (kbd "C-<tab>") 'previous-buffer)
(global-set-key (kbd "<f10>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)

;; This mode looks way cool. Let's use it
;; We shall never lose the cursor again.!
;; URL: http://endlessparentheses.com/beacon-never-lose-your-cursor-again.html
(use-package beacon
  :ensure t
  :config
  (beacon-mode)
  (setq beacon-push-mark 35)
  (setq beacon-color "#F92672")
  :diminish 'beacon-mode)

;; Maximize and minimize windows. I used to have them in C-c C-m and
;; C-c C-M, but I found out that's no good. I haven't even made use of
;; those commands, but if I even need them I'll leave them at C-c +
;; and C-c -
(global-set-key (kbd "C-c +") 'maximize-window)
(global-set-key (kbd "C-c -") 'minimize-window)


;; Change window with F12 should be good
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-<f12>") (lambda () (interactive) (other-window -1)))

;; Some bindings from https://www.masteringemacs.org/article/my-emacs-keybindings
(global-set-key (kbd "M-o") 'other-window)

;; I want to be able to scroll without moving the point
;; source: https://www.emacswiki.org/emacs/Scrolling
(defun my/scrolldown (n)
  "Scroll down N lines without moving the point."
  (interactive "p")
  (dotimes (i n)
    (scroll-up-command 1)))

(defun my/scrollup (n)
  "Scroll up N lines without moving the point."
  (interactive "p")
  (dotimes (i n)
    (scroll-down-command 1)))

(global-set-key (kbd "M-n") 'my/scrolldown)
(global-set-key (kbd "M-p") 'my/scrollup)



(provide 'setup-ui)
;;; setup-ui.el ends here
