;;; setup-file-nav.el --- Set up tools to navigate through files.
;;; Commentary:
;;; Code:

;; NeoTree could *sometimes* be better than Dired.
(use-package neotree
  :ensure t
  :config
  (defun ach-neotree-toggle ()
    "If there's a projectile project going on, open neotree at project root."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (let* ((this-file (f-this-file))
             (this-path (when this-file
                          (f-dirname (f-this-file))))
             (this-project (when (projectile-project-p)
                             (projectile-project-root))))
        (neotree-show)
        (if this-project
            (neotree-dir this-project)
          (when this-file
            (neotree-dir this-path))))))
  (global-set-key (kbd "<f7>") #'ach-neotree-toggle)
  (define-key meta-m-map (kbd "M-d") #'ach-neotree-toggle))

;; This package is cool, I like coloring stuff around
(use-package dired-k
  :ensure t
  :config
  (progn
    (add-hook 'dired-initial-position-hook 'dired-k)
    (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))

;; it seems sensible to hide every detail by default and use ) when I
;; have to show more info, like file size or permissions
(use-package dired+
  :ensure t
  :config
  (global-dired-hide-details-mode 1)
  ;; Since b is unbound in dired and ^ is a very annoying key to press,
  ;; lets bind that to dired-up-directory
  (with-eval-after-load 'dired+
    (define-key dired-mode-map (kbd "b") #'dired-up-directory))
  (defun ach-dired-search ()
    (interactive)
    (goto-char (point-min))
    (call-interactively 'isearch-forward))
  (with-eval-after-load 'dired+
    (define-key dired-mode-map (kbd "/") #'ach-dired-search)))

;; We want to be able to toggle dot files in dired
(add-hook 'dired-mode-hook (lambda ()
                             (require 'dired-x)
                             (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
                             (define-key dired-mode-map "h" #'dired-omit-mode)))

;; Using "a" in dired is way more sensible than f.
(put 'dired-find-alternate-file 'disabled nil)

;; ffap seemed cool but i wanna use the guess
(require 'ffap)
(defun ach-go-to-file-at-point ()
  "Jump to file at point."
  (interactive)
  (let ((guess (ffap-guesser)))
    (unless guess
      (error "No file at point"))
    (find-file guess)))

(global-set-key (kbd "C-c g") 'ach-go-to-file-at-point)

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

;; Jump to init.el
(defun ach-jump-to-init ()
  "Jump directly to init.el in current buffer."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(ach-define-bookmark "~/.emacs.d/init.el" "i" "emacs-init")

(provide 'setup-file-nav)
;;; setup-file-nav.el ends here
