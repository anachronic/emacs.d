;;; setup-ui.el --- UI related configuration.
;;; Commentary:
;;; Code:

;; get rid of everything other than the title at the top.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; these two come from: https://www.emacswiki.org/emacs/SmoothScrolling
;; (setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; get rid of the annoying GNU Emacs buffer
(setq inhibit-startup-screen t)

;; also get rid of the scratch buffer message
(setq initial-scratch-message nil)

;; load the latest theme.
;; (load-theme 'monokai t)
(load-theme 'avk-darkblue-white t)
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

;; The licensed code below has a BSD license which requires me to
;; include the following:

;; Copyright (c) 2006-2014, Steve Purcell
;; All rights reserved.

;; Actually I'm not trying to redistribute this code or anything, but
;; I feel like it's healthier

;; I've been stealing from Steve Purcell's config a lot. So let's
;; tweak ibuffer to show human readable sizes
(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))


(with-eval-after-load 'ibuffer
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

;; Purcell's ibuffer-vc. I actually use a lot more helm-mini, but
;; sometimes I get lost into how many buffers I open, so this can be
;; handy in that situation.
(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  :config
  (require 'ibuffer-vc)
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook 'ibuffer-hook #'ibuffer-set-up-preferred-filters))

(provide 'setup-ui)
;;; setup-ui.el ends here
