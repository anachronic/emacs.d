;;; setup-ui.el --- UI related configuration.
;;; Commentary:
;;; Code:

;; get rid of everything other than the title at the top.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; these two come from: https://www.emacswiki.org/emacs/SmoothScrolling
;; (setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

;; also get rid of the scratch buffer message
(setq initial-scratch-message nil)

;; load the latest theme.
(load-theme 'nsalas-tomorrow-night t)
(set-face-attribute 'default nil :height 105)

;; On my laptop I'd rather have a bigger text
(require 's)
(when (s-equals? (system-name) "okinawa")
  (set-face-attribute 'default nil :height 110))

;; I ran into a VERY weird issue where C-n is the slowest thing
;; ever. Fortunately, I found an answer
;; https://lists.gnu.org/archive/html/emacs-devel/2006-09/msg00634.html
;; (setq auto-window-vscroll nil)

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

;; no DING!
(setq visible-bell 1)

;; This has been driving me crazy. So new key binding.
(global-set-key (kbd "<f9>") 'split-window-right)

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

;; I'll rebind these to C-v and M-v someday
;; (global-set-key (kbd "M-n") 'my/scrolldown)
;; (global-set-key (kbd "M-p") 'my/scrollup)

;; This one was recommended by Steve Purcell. Looked pretty good
;; From this chat: https://www.youtube.com/watch?v=Gq0hG_om9xY
(use-package fullframe
  :ensure t
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe ibuffer ibuffer-quit)
  (fullframe package-list-packages quit-window)
  (fullframe list-packages quit-window))

(provide 'setup-ui)
;;; setup-ui.el ends here
