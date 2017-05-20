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

;; If powerline fonts are installed, use that
(let ((pl-font "DejaVu Sans Mono for Powerline"))
  (when (member pl-font (font-family-list))
    (set-face-attribute 'default nil :font pl-font)))

(require 's)
(set-face-attribute 'default nil :height 110)

;; I ran into a VERY weird issue where C-n is the slowest thing
;; ever. Fortunately, I found an answer
;; https://lists.gnu.org/archive/html/emacs-devel/2006-09/msg00634.html
;; Actually this, for some reason, makes the C-n be very fast even in
;; very large buffers. Weird
(setq auto-window-vscroll nil)

;; no DING!
(setq visible-bell 1)

;; This has been driving me crazy. So new key binding.
(global-set-key (kbd "<f9>") 'split-window-right)

;; This mode looks way cool. Let's use it
;; We shall never lose the cursor again.!
;; URL: http://endlessparentheses.com/beacon-never-lose-your-cursor-again.html
(use-package beacon
  :ensure t
  :demand
  :bind (("C-x =" . beacon-blink))
  :config
  (beacon-mode)
  (setq beacon-push-mark 35)
  (setq beacon-color "#32e006")
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
;; Let's combine it with window-numbering
(defun nsv/other-window (arg)
  "Switch to ARGth window (as in window-numbered), if not arg, regular `other-window'."
  (interactive "P")
  (if (or (not arg)
          (listp arg)
          (< arg 0)
          (> arg 9))
      (other-window 1)
    (select-window-by-number arg)))

(global-set-key (kbd "M-o") 'nsv/other-window)

;; I want to be able to scroll without moving the point source:
;; https://www.emacswiki.org/emacs/Scrolling

;; First, define a variable that decides if movement is per line or
;; per "page"
(defvar nsv/scroll-per-line t
  "Set to t if scroll should be per line instead of per page.")

;; The next 2 functions will default scrolling to 1 line per key
;; hit. Calling with unnumbered prefix will change their scrolling
;; style to page/line depending on the previous value.
(defun nsv/scrolldown (n)
  "Scroll down N lines without moving the point.

With an unnumbered prefix, toggle between scrolling style."
  (interactive "P")
  (when (and n (listp n))
    (setq nsv/scroll-per-line (not nsv/scroll-per-line)))
  (if nsv/scroll-per-line
      (progn
        (when (or (listp n) (not n))
          (setq n 1))
        (dotimes (i n)
          (scroll-up-command 1)))
    (scroll-up-command)))

(defun nsv/scrollup (n)
  "Scroll up N lines without moving the point.

With an unnumbered prefix, toggle between scrolling style."
  (interactive "P")
  (when (and n (listp n))
    (setq nsv/scroll-per-line (not nsv/scroll-per-line)))
  (if nsv/scroll-per-line
      (progn
        (when (or (listp n) (not n))
          (setq n 1))
        (dotimes (i n)
          (scroll-down-command 1)))
    (scroll-down-command)))


(global-set-key (kbd "C-v") 'nsv/scrolldown)
(global-set-key (kbd "M-v") 'nsv/scrollup)

;; This one was recommended by Steve Purcell. Looked pretty good
;; From this chat: https://www.youtube.com/watch?v=Gq0hG_om9xY
(use-package fullframe
  :ensure t
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe magit-status-internal magit-mode-quit-window)
  (fullframe ibuffer ibuffer-quit)
  (fullframe package-list-packages quit-window)
  (fullframe list-packages quit-window))

;; From https://www.emacswiki.org/emacs/TransposeWindows
;; and crux.
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows.

Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(define-key meta-m-map (kbd "t") 'transpose-windows)

(provide 'setup-ui)
;;; setup-ui.el ends here
