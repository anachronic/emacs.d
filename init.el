;;; package --- Summary
;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; General paths. Self-written scripts and themes.
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Nobody likes those annoying ~ end files, so redirect them.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Base auto-install and pretty code for configuring packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Helm Config
;; Completion is HISTORICALLY bound to TAB on almost ANY editor. So we do that here.
(use-package helm
  :ensure t
  :config
  (progn
    (helm-mode 1)
    (setq helm-mode-fuzzy-match t)
    (require 'helm-config)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring))
  :diminish ""
  :demand)

;; Magit is critical for any developer
(use-package magit
  :ensure t
  :bind (("<f8>" . magit-status)
	 ("s-t" . magit-status)
         ("C-x g" . magit-status)))

;; Company: Not much customization right now.
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (setq company-idle-delay 0.3)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map [tab] 'company-complete-common-or-cycle))
  :diminish ""  ;; it is almost always on anyway.
  :bind (("C-S-<SPC>" . company-complete)))

;; Some help can't hurt
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;; Company statistics. I do use this a lot, maybe i should't be coding like I do...
(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode))

;; Will be trying company-flx for a while.
(use-package company-flx
  :ensure t
  :config
  (company-flx-mode +1))


;; Helm fuzzy mode doesn't seem to be as good as smex...
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

;; Autopair lets you code with no worries.
;; *especially* elisp, or any lisp based programming language
(use-package autopair
  :ensure t
  :init
  (autopair-global-mode)
  :diminish "")

;; YASnippet, always so handy...
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode ;; well, this is always on, so..
  :init
  (yas-global-mode 1)
  :config
  (progn
    (yas-reload-all)
    (define-key yas-minor-mode-map (kbd "C-<return>") 'yas-exit-snippet)
    (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)))
    ;; (define-key yas-minor-mode-map (kbd "C-m") 'yas-exit-snippet)))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Undo-Tree for real undo/redo commands
(use-package undo-tree
  :ensure t
  :diminish ""
  :init
  (global-undo-tree-mode 1)
  :bind
  (("C-z" . undo)
   ("C-S-z" . undo-tree-redo)))

;;;;; At this point I feel like im just copying people..
;;;;; thats fine though

;; ace-window from Howard Abrams. I hear its nice
(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "C-x o") 'ace-window))

;; Projectile, for projects
(use-package projectile
  :ensure t
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'helm)
    (projectile-global-mode))
  :config
  (setq projectile-enable-caching t))


;; Flycheck. What's an editor without error checking?
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; AUCTeX: This is critical for me and LaTeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode))

;; We'd also like to instantly preview our TeX docs.
;; It's quite nice that latex-preview-pane works well with DocView
(use-package latex-preview-pane
  :ensure t
  :config
  (latex-preview-pane-enable))

;; yaml-mode. mainly for syntax highlighting
(use-package yaml-mode
  :ensure t ;; seems like overkill
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;; expand region. An *excellent* tool.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Visual fill column for text-only buffers, like mail and feeds. and maybe org..
(use-package visual-fill-column
  :ensure t)

;; ace-link is a great way of jumping around links.
(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))


;; Shell pop config
(use-package shell-pop
  :ensure t
  :demand)

;; which-key seems like a really nice help
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :demand
  :diminish which-key-mode)

;; s.el is useful for our own functions
(use-package s
  :ensure t)

;; As of this point I feel like: YEAH!! LET'S COLOUR EVERYTHING!
(use-package dired-k
  :ensure t
  :config
  (progn
    (add-hook 'dired-initial-position-hook 'dired-k)
    (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))

;; evil is good for navigation, lets give it a key.
(use-package evil)

(global-set-key (kbd "<f6>") (lambda (x) (interactive "p") (call-interactively 'evil-mode)))

;; We have all our modes set.
;; Simple config goes hardcoded here.
;; Grouped configuration files can be found in ./elisp

;; Add stuff to text so you get visual aid when coding
(require 'setup-editor)

;; Visual configuration. Here you'll find stuff about how Emacs looks
(require 'setup-ui)

;; Renaming an open buffer file.
(require 'renamefile)

;; Need an org mode config file. For now it's small but I'm sure it'll
;; get bigger.
(require 'setup-org)

;; Recentf config.
(require 'setup-recentf)

;; Eshell config.
(require 'setup-eshell)

;; C programming requires a special section
(require 'c-programming)

;; Mail conf
(require 'setup-mail)

;; News reading conf
(require 'setup-news)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-irony-c-headers company-irony flycheck-irony irony evil elfeed-goodies ace-link evil-nerd-commenter latex-preview-pane helm-gtags yasnippet yaml-mode which-key visual-fill-column use-package undo-tree smex smart-comment shell-pop projectile powerline nlinum-relative markdown-mode magit helm flycheck expand-region elfeed direx company-statistics company-quickhelp company-flx color-identifiers-mode autopair auctex ace-window)))
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (file-remote-p default-directory)
         " Projectile"
       (format " Proj[%s]"
               (projectile-project-name))))))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "<f5>"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monospace" :foundry "1ASC" :slant normal :weight normal :height 105 :width normal)))))

;; just to suppress warnings.
(provide 'init)
;;; init.el ends here
