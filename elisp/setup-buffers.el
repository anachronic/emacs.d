;;; setup-buffers.el --- Buffer moving or killing defuns.
;;; Commentary:
;;; Code:

;; The auto revert lighter seems unnecessary.
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

;; So i was using kill-this-buffer for C-x k. But sometimes I don't really
;; want to kill the buffer, I just want it to get it out of the way.
;; So let's bury the buffer with the same key when a prefix is specified.
(defun nsv/kill-buffer-or-bury-dwim (&optional arg)
  "If ARG, bury buffer, otherwise kill the buffer."
  (interactive "P")
  (if arg
      (bury-buffer)
    (kill-this-buffer)))

(define-key ctl-x-map (kbd "k") 'nsv/kill-buffer-or-bury-dwim)

;; According to http://oremacs.com/2015/02/18/undo-nonsense/, find-file-read-only
;; is a trashy command. Whatever. Who cares about useless commands, we don't even use
;; the useful ones sometimes, right?
;; That's right. But hold on. This trashy command is bound to C-x C-r. Let's get
;; rid of it and bind it to a useful command: revert buffer
(defun nsv/revert-buffer ()
  "Revert buffer without asking if you really want to."
  (interactive)
  (revert-buffer nil t))

(global-set-key (kbd "C-x C-r") #'nsv/revert-buffer)

;; I've been burying buffers like crazy lately because it feels more
;; natural than killing them. However, I'd like to have the
;; possibility to bury the current buffer with C-q and bury the other
;; buffer with a prefix argument. This will override quoted insert,
;; but that's ok since i hardly ever use it. Let's just rebind that to
;; C-c q
(defun nsv/bury-buffer-dwim (arg)
  "Bury current buffer, if ARG is not nil, switch to last buffer."
  (interactive "P")
  (if arg
      (previous-buffer)
    (bury-buffer)))

(global-set-key (kbd "C-q") 'nsv/bury-buffer-dwim)
(global-set-key (kbd "C-c q") 'quoted-insert)


;; Frame commands
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "C-c o") 'other-frame)

(global-set-key (kbd "C-c n")
                (lambda ()
                  (interactive)
                  (make-frame)
                  (other-frame)))

;; emacs-close-dwim
;; I want to C-x C-c out of a frame without closing emacs.
;; So if there's more than one frame active. Just close it
;; If there's only one, shut down emacs. And hell, don't ask
;; to save buffers, just do it.
(defun nsv/emacs-close-dwim ()
  "Close current frame if there's more than one active.
Otherwise exit Emacs."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))

;; rebind it to C-x C-c
(define-key ctl-x-map (kbd "C-c") 'nsv/emacs-close-dwim)

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
(windmove-default-keybindings 'control)

;; Movement hydra: Vertical enlarge/shrink is kinda confusing atm
(defhydra hydra-window (:color red :hint nil)
  "
Movement^^      ^Split^             ^Resize^    ^Winner^
--------------------------------------------------------
_j_ ←           _v_ertical          _J_ X←      _u_ndo
_k_ ↓           _h_orizontal        _K_ X↓      _r_edo
_i_ ↑           _0_ delete current  _I_ X↑
_l_ →           _1_ delete others   _L_ X→
_q_uit
"
  ("j" windmove-left)
  ("k" windmove-down)
  ("i" windmove-up)
  ("l" windmove-right)
  ("I" enlarge-window)
  ("L" enlarge-window-horizontally)
  ("J" shrink-window-horizontally)
  ("K" shrink-window)
  ("1" delete-other-windows)
  ("v" (funcall (global-key-binding "\C-x3")))
  ("h" (funcall (global-key-binding "\C-x2")))
  ("0" delete-window)
  ("u" winner-undo)
  ("r" winner-redo)
  ("q" nil))

(global-set-key (kbd "C-c w") 'hydra-window/body)

;; Some people swear by winner mode
(winner-mode 1)

;; Let's bind it to accesible keys.
(global-set-key (kbd "C-M-<") #'winner-undo)
(global-set-key (kbd "C-M->") #'winner-redo)

;; uniquify. I really got used to IntelliJ idea's way of handling stuff
(setq uniquify-buffer-name-style 'forward)

(provide 'setup-buffers)
;;; setup-buffers.el ends here
