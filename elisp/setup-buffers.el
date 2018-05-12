;;; setup-buffers.el --- Buffer moving or killing defuns.
;;; Commentary:
;;; Code:

;; The auto revert lighter seems unnecessary.
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(define-key ctl-x-map (kbd "k") 'kill-this-buffer)

;; http://oremacs.com/2015/02/18/undo-nonsense/
;; C-x C-r => revert buffer without asking
(defun ach-revert-buffer ()
  "Revert buffer without asking if you really want to."
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "C-x C-r") #'ach-revert-buffer)

(global-set-key (kbd "C-q") 'bury-buffer)
(global-set-key (kbd "C-c q") 'quoted-insert)

;; use ibuffer instead of default C-x C-b
(define-key ctl-x-map (kbd "C-b") 'ibuffer)

;; Some people swear by winner mode
(winner-mode 1)

;; Let's bind it to accesible keys.
(global-set-key (kbd "C-M-<") #'winner-undo)
(global-set-key (kbd "C-M->") #'winner-redo)

;; uniquify. I really got used to IntelliJ idea's way of handling stuff
(setq uniquify-buffer-name-style 'forward)

(provide 'setup-buffers)
;;; setup-buffers.el ends here
