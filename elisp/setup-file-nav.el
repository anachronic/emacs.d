;;; setup-file-nav.el --- Set up tools to navigate through files.
;;; Commentary:
;;; Code:

;; This package is cool, I like coloring stuff around
;; Dired-k seems to kill git processes. It's actually very annoying.
;; see https://github.com/syohex/emacs-dired-k/issues/45
;; (use-package dired-k
;;   :ensure t
;;   :config
;;   (progn
;;     (add-hook 'dired-initial-position-hook 'dired-k)
;;     (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))

(defun dired-xdg-open ()
  "In dired, open the file named on this line."
  (interactive)
  (condition-case nil
      (let* ((file (dired-get-filename nil t)))
        (call-process "xdg-open" nil 0 nil file))
    (error (progn
             (ding)
             (message "Can't xdg-open file at point")))))

(define-key dired-mode-map (kbd "M-RET") 'dired-xdg-open)
(evil-define-minor-mode-key 'normal 'dired-mode
  (kbd "-") 'dired-up-directory)

;; We want to be able to toggle dot files in dired
;; (add-hook 'dired-mode-hook (lambda ()
;;                              (require 'dired-x)
;;                              (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;;                              (define-key dired-mode-map "h" #'dired-omit-mode)))

;; Using "a" in dired is way more sensible than f.
(put 'dired-find-alternate-file 'disabled nil)

;; I want a function to chmod 755 this file
;; No hotkey for it, just M-x it
(defun chmod-755-this-file ()
  "Chmod 755 this file."
  (interactive)
  (let ((this-file (buffer-file-name)))
    (when (or (not this-file)
              (not (file-exists-p this-file)))
      (error "No file found"))
    (set-file-modes this-file (string-to-number "755" 8))))

(provide 'setup-file-nav)
;;; setup-file-nav.el ends here
