;;; setup-ui.el --- UI related configuration.
;;; Commentary:
;;; Code:

;; https://www.emacswiki.org/emacs/SmoothScrolling
;; Move the lines as they're should with the mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; load the latest theme.
(load-theme 'ach-dark t)

;; If powerline fonts are installed, use that
(let ((pl-font "DejaVu Sans Mono"))
  (when (member pl-font (font-family-list))
    (set-face-attribute 'default nil :font pl-font)))

(defun ach-set-font-height-125 ()
  "Set font height to 125."
  (interactive)
  (set-face-attribute 'default nil :height 125))

(set-face-attribute 'default nil
                    :height 110)
(use-package fullframe
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window)
  (fullframe magit-status-internal magit-mode-quit-window)
  (fullframe ibuffer ibuffer-quit)
  (fullframe package-list-packages quit-window)
  (fullframe list-packages quit-window))

;; Eyebrowse for something more tmux'y
(use-package eyebrowse
  :ensure t
  :init
  (setq-default eyebrowse-keymap-prefix (kbd "C-x x"))
  (setq-default eyebrowse-mode-line-style 'always)
  :config
  (eyebrowse-mode)
  (define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (define-key eyebrowse-mode-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (define-key eyebrowse-mode-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
  (define-key evil-motion-state-map (kbd "gt") 'eyebrowse-next-window-config)
  (define-key evil-motion-state-map (kbd "gT") 'eyebrowse-prev-window-config)
  (define-key evil-motion-state-map (kbd "C-SPC") 'eyebrowse-last-window-config)
  (define-key evil-motion-state-map (kbd "zx") 'eyebrowse-last-window-config))

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

(provide 'setup-ui)
;;; setup-ui.el ends here
