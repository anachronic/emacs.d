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

;; uniquify. I really got used to IntelliJ idea's way of handling stuff
(setq uniquify-buffer-name-style 'forward)

(with-eval-after-load 'with-editor
  (diminish 'with-editor-mode ""))

;; sudo save a buffer
;; workflow is: open the file and C-x C-q it (disable read-only
;; mode). Make your changes and M-x sudo-save RET. Input password and
;; profit.
(defun sudo-save ()
  "Save current file as sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Rename file and buffer
;; from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(provide 'setup-buffers)
;;; setup-buffers.el ends here
