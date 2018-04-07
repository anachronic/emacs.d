;;; setup-ui.el --- UI related configuration.
;;; Commentary:
;;; Code:


;; For EWW buffers to actually be visible
;; Source: https://emacs.stackexchange.com/a/3523
(with-eval-after-load 'shr-color
  (setq shr-color-visible-luminance-min 70))

;; these two come from: https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; load the latest theme.
;; (load-theme 'nsalas-tomorrow-night t)
(load-theme 'nsalas-flatui t)

;; If powerline fonts are izstalled, use that
(let ((pl-font "DejaVu Sans Mono"))
  (when (member pl-font (font-family-list))
    (set-face-attribute 'default nil :font pl-font)))

(defun ach--get-font-height-for-host (host)
  "Return a sensible font size for HOST."
  (cond
   ((string= host "okinawa") 110)
   (t 110)))

(defun ach-set-font-height-125 ()
  "Set font height to 125."
  (interactive)
  (set-face-attribute 'default nil :height 125))

(set-face-attribute 'default nil
                    :height (ach--get-font-height-for-host
                             (system-name)))

;; This has been driving me crazy. So new key binding.
(global-set-key (kbd "<f9>") 'split-window-right)

;; This mode looks way cool. Let's use it
;; We shall never lose the cursor again.!
;; URL: http://endlessparentheses.com/beacon-never-lose-your-cursor-again.html
(use-package beacon
  :ensure t
  :demand
  :bind (("C-x =" . beacon-blink))
  :init
  (add-hook 'comint-mode-hook (lambda () (beacon-mode -1)))
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

(global-set-key (kbd "M-o") 'other-window)

;; First, define a variable that decides if movement is per line or
;; per "page"
(defvar ach-scroll-per-line t
  "Set to t if scroll should be per line instead of per page.")

;; The next 2 functions will default scrolling to 1 line per key
;; hit. Calling with unnumbered prefix will change their scrolling
;; style to page/line depending on the previous value.
(defun ach-scrolldown (n)
  "Scroll down N lines without moving the point.

With an unnumbered prefix, toggle between scrolling style."
  (interactive "P")
  (when (and n (listp n))
    (setq ach-scroll-per-line (not ach-scroll-per-line)))
  (if ach-scroll-per-line
      (progn
        (when (or (listp n) (not n))
          (setq n 1))
        (dotimes (i n)
          (scroll-up-command 1)))
    (scroll-up-command)))

(defun ach-scrollup (n)
  "Scroll up N lines without moving the point.

With an unnumbered prefix, toggle between scrolling style."
  (interactive "P")
  (when (and n (listp n))
    (setq ach-scroll-per-line (not ach-scroll-per-line)))
  (if ach-scroll-per-line
      (progn
        (when (or (listp n) (not n))
          (setq n 1))
        (dotimes (i n)
          (scroll-down-command 1)))
    (scroll-down-command)))


(global-set-key (kbd "C-v") 'ach-scrolldown)
(global-set-key (kbd "M-v") 'ach-scrollup)

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
  (fullframe list-packages quit-window)
  (fullframe shell bury-buffer)
  )

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



;; Indent guide. Looks like the best option without breaking the UI
(autoload 'indent-guide-mode "indent-guide")
(add-hook 'python-mode-hook 'indent-guide-mode)
(add-hook 'ruby-mode-hook 'indent-guide-mode)
(add-hook 'js2-mode-hook 'indent-guide-mode)
(add-hook 'web-mode-hook 'indent-guide-mode)

(with-eval-after-load 'indent-guide
  (setq indent-guide-delay 0.1)
  (diminish 'indent-guide-mode))

;; Highlighting. I kind of like this feature
(global-hi-lock-mode)
(diminish 'hi-lock-mode)

(provide 'setup-ui)
;;; setup-ui.el ends here
