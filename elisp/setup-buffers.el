;;; setup-buffers.el --- Buffer moving or killing defuns.
;;; Commentary:
;;; Code:

;; So i was using kill-this-buffer for C-x k. But sometimes I don't really
;; want to kill the buffer, I just want it to get it out of the way.
;; So let's bury the buffer with the same key when a prefix is specified.
(defun my/kill-buffer-or-bury-dwim (&optional arg)
  "If ARG, bury buffer, otherwise kill the buffer."
  (interactive "P")
  (if arg
      (bury-buffer)
    (kill-this-buffer)))

(define-key ctl-x-map (kbd "k") 'my/kill-buffer-or-bury-dwim)

;; According to http://oremacs.com/2015/02/18/undo-nonsense/, find-file-read-only
;; is a trashy command. Whatever. Who cares about useless commands, we don't even use
;; the useful ones sometimes, right?
;; That's right. But hold on. This trashy command is bound to C-x C-r. Let's get
;; rid of it and bind it to a useful command: revert buffer
(defun my/revert-buffer ()
  "Revert buffer without asking if you really want to."
  (interactive)
  (revert-buffer nil t))

(global-set-key (kbd "C-x C-r") #'my/revert-buffer)

;; I've been burying buffers like crazy lately because it feels more
;; natural than killing them. However, I'd like to have the
;; possibility to bury the current buffer with C-q and bury the other
;; buffer with a prefix argument. This will override quoted insert,
;; but that's ok since i hardly ever use it. Let's just rebind that to
;; C-c q
(defun my/bury-buffer-dwim (arg)
  "Bury current buffer, if ARG is not nil, bury other-window's buffer instead."
  (interactive "P")
  (if arg
      (progn
        (other-window 1)
        (bury-buffer)
        (other-window -1))
    (bury-buffer)))

(global-set-key (kbd "C-q") 'my/bury-buffer-dwim)
(global-set-key (kbd "C-c q") 'quoted-insert)


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

;; I was using ace window, but trying out purcell's config I realized
;; switch-window can be better
(use-package switch-window
  :ensure t
  :demand
  :bind (("C-x o" . switch-window))
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-timeout nil))

;; And rebind C-x 2 and C-x 3 to behave good
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall s-f)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window))))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

;; I've actually not used shift+arrows in a while, so let's bind
;; windmove to that
(windmove-default-keybindings)


(provide 'setup-buffers)
;;; setup-buffers.el ends here
