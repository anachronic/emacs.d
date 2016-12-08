;;; setup-file-nav.el --- Set up tools to navigate through files.
;;; Commentary:
;;; Code:

;; direx looks like a good alternative. I have tried NeoTree, but it sucks.
(use-package direx
  :ensure t)

;; NeoTree could *sometimes* be better than Dired.
(use-package neotree
  :ensure t
  :config
  (defun my/neotree-toggle ()
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
  (global-set-key (kbd "<f7>") #'my/neotree-toggle)
  (define-key meta-m-map (kbd "M-d") #'my/neotree-toggle))

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
    (define-key dired-mode-map (kbd "b") #'dired-up-directory)))

;; We want to be able to toggle dot files in dired
(add-hook 'dired-mode-hook (lambda ()
                             (require 'dired-x)
                             (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
                             (define-key dired-mode-map "h" #'dired-omit-mode)))

;; Using "a" in dired is way more sensible than f.
(put 'dired-find-alternate-file 'disabled nil)

(provide 'setup-file-nav)
;;; setup-file-nav.el ends here
