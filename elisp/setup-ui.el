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

;; On my laptop I'd rather have a bigger text
(require 's)
(when (s-equals? (system-name) "okinawa")
  (set-face-attribute 'default nil :height 110))

;; I ran into a VERY weird issue where C-n is the slowest thing
;; ever. Fortunately, I found an answer
;; https://lists.gnu.org/archive/html/emacs-devel/2006-09/msg00634.html
;; Actually this, for some reason, makes the C-n be very fast even in
;; very large buffers. Weird
(setq auto-window-vscroll nil)

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
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-buffer-position-off)

;; This package looks ok
(use-package window-numbering
  :ensure t
  :demand
  :config
  (window-numbering-mode)
  (window-numbering-clear-mode-line)
  (define-key window-numbering-keymap (kbd "M-0") nil)
  (define-key window-numbering-keymap (kbd "M-1") nil)
  (define-key window-numbering-keymap (kbd "M-2") nil)
  (define-key window-numbering-keymap (kbd "M-3") nil)
  (define-key window-numbering-keymap (kbd "M-4") nil)
  (define-key window-numbering-keymap (kbd "M-5") nil)
  (define-key window-numbering-keymap (kbd "M-6") nil)
  (define-key window-numbering-keymap (kbd "M-7") nil)
  (define-key window-numbering-keymap (kbd "M-8") nil)
  (define-key window-numbering-keymap (kbd "M-9") nil)
  (define-key window-numbering-keymap (kbd "M-0") nil)
  (define-key window-numbering-keymap (kbd "C-M-0") 'select-window-0)
  (define-key window-numbering-keymap (kbd "C-M-1") 'select-window-1)
  (define-key window-numbering-keymap (kbd "C-M-2") 'select-window-2)
  (define-key window-numbering-keymap (kbd "C-M-3") 'select-window-3)
  (define-key window-numbering-keymap (kbd "C-M-4") 'select-window-4)
  (define-key window-numbering-keymap (kbd "C-M-5") 'select-window-5)
  (define-key window-numbering-keymap (kbd "C-M-6") 'select-window-6)
  (define-key window-numbering-keymap (kbd "C-M-7") 'select-window-7)
  (define-key window-numbering-keymap (kbd "C-M-8") 'select-window-8)
  (define-key window-numbering-keymap (kbd "C-M-9") 'select-window-9))

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
(defun my/other-window (arg)
  "Switch to ARGth window (as in window-numbered), if not arg, regular `other-window'."
  (interactive "p")
  (if (or (not arg)
          (< arg 0)
          (> arg 9))
      (other-window 1)
    (select-window-by-number arg)))

(global-set-key (kbd "M-o") 'my/other-window)

;; I want to be able to scroll without moving the point source:
;; https://www.emacswiki.org/emacs/Scrolling Actually, I'll make these
;; interactive "P", so that C-u prefix will behave like a normal C-v
;; and same with M-v
(defun my/scrolldown (n)
  "Scroll down N lines without moving the point.

With an unnumbered prefix, do a normal call to scroll up command."
  (interactive "P")
  (if (and n (listp n))
      (scroll-up-command)
    (when (not n) (setq n 1))
    (dotimes (i n)
      (scroll-up-command 1))))

(defun my/scrollup (n)
  "Scroll up N lines without moving the point.

With unnumbered prefix do normal call to scroll down command."
  (interactive "P")
  (if (and n (listp n))
      (scroll-down-command)
    (when (not n) (setq n 1))
    (dotimes (i n)
      (scroll-down-command 1))))

(global-set-key (kbd "C-v") 'my/scrolldown)
(global-set-key (kbd "M-v") 'my/scrollup)

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
