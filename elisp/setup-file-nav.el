;;; setup-file-nav.el --- Set up tools to navigate through files.
;;; Commentary:
;;; Code:

;; direx looks like a good alternative. I have tried NeoTree, but it sucks.
(use-package direx
  :ensure t)

(defun direx:collapse-current ()
  "Collapse the current item's parent."
  (interactive)
  (direx:up-item)
  (direx:toggle-item))

;; Jump to directory: I'd like to use direx if there's a projectile
;; session going on and fallback to dired if there's none.
(defun my/directory-jump ()
  "Jump to direx on project root if a project is active, dired otherwise."
  (interactive)
  (if (projectile-project-p)
      (direx-project:jump-to-project-root)
    (dired-jump)))

;; Jump to dired pointing at current direx file
(defun my/dired-jump-from-direx ()
  "Jump from direx to dired."
  (interactive)
  (let ((path (aref (direx:item-tree (direx:item-at-point!)) 2)))
    (dired-jump nil path)))

(with-eval-after-load 'direx
  (define-key direx:direx-mode-map (kbd "b") #'direx:collapse-current)
  ;; I'd also like to go to dired-mode from direx
  (define-key direx:direx-mode-map (kbd "C-x C-j") #'my/dired-jump-from-direx)
  (define-key direx:direx-mode-map (kbd "s") #'counsel-git))

(with-eval-after-load 'dired-x
  (global-unset-key (kbd "C-x C-j"))
  (global-unset-key (kbd "C-x d"))
  (define-key ctl-x-map (kbd "C-j") 'my/directory-jump)
  (define-key ctl-x-map (kbd "d") 'dired-jump))

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
