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
  :bind
  (("C-S-m" . helm-mini)
   ("C-x b" . helm-mini))
  :config
  (progn
    (helm-mode 1)
    (setq helm-mode-fuzzy-match t)
    (setq helm-ff-newfile-prompt-p nil)
    (require 'helm-config)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key [remap occur] 'helm-occur)

    ;; I was using counsel to find files. But since ocasionally I do have
    ;; to edit some system files, Helm will do the trick. This disables
    ;; which-key and projectile globally, which is a pain in the ass.
    ;; It's ok though. I don't do this very often.
    ;; sources:
    ;; advice: http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
    ;; Projectile & wk: http://emacs.stackexchange.com/questions/4234/cannot-edit-a-file-as-root-if-authinfo-gpg-file-exists
    (defadvice helm-find-files (after find-file-sudo activate)
      "Find file as root if necessary."
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (projectile-mode -1)
        (which-key-mode -1)
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))
  :diminish ""
  :demand)

;; I've come to think helm is not really good with files anymore. So let's
;; use counsel. It also has some nice builtin functionality:
;; http://oremacs.com/2015/04/19/git-grep-ivy/
(use-package counsel
  :ensure t
  :after helm
  :config
  (global-set-key (kbd "C-.") 'counsel-imenu)
  (global-set-key (kbd "C-c s") 'counsel-grep)
  (global-set-key (kbd "C-c g") 'counsel-git-grep)
  (global-set-key (kbd "C-c M-x") 'counsel-M-x))

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
  :bind (("M-x"    . smex)
         ("M-X"    . smex-major-mode-commands)
         ("<menu>" . smex)))

;; YASnippet, always so handy...
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode ;; well, this is always on, so..
  :config
  (progn
    (yas-global-mode 1)
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
  :config
  (global-undo-tree-mode 1)
  :bind
  (("C-z" . undo)
   ("C-S-z" . undo-tree-redo)))

;;;;; At this point I feel like im just copying people..
;;;;; thats fine though

;; ace-window from Howard Abrams. I hear its nice
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package ivy
  :ensure t)

;; Lets use swiper conservatively. I actually like isearch better.
(use-package swiper
  :ensure t
  :bind ("C-S-s" . swiper))

;; Projectile, for projects
(use-package projectile
  :ensure t
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'helm)
    (projectile-global-mode))
  :config
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-dired))

;; Helm projectile. I'm surprised I didn't install this before.
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))

;; Helm is very cool for everything. So is counsel/avy/etc...
;; However: Helm is __terrible__ when dealing with file handling. Whether it be finding a file
;; (especially a directory), or worse, saving a file. Helm is a pain in the ass
;; So we'll use counsel to save and load files.
;; On the other hand. No package is better for occur/grep than helm.
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile helm-projectile)
  :config
  (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
  (define-key projectile-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
  (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project))

;; Flycheck. What's an editor without error checking?
(use-package flycheck
  :ensure t
  :diminish "FlyC" ;; Errors and warnings appear in the modeline anyway
  :config
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; yaml-mode. mainly for syntax highlighting
(use-package yaml-mode
  :ensure t ;; seems like overkill
  :config
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

;; This is GREAT when tags don't really cut it
(use-package dumb-jump
  :ensure t
  :diminish (dumb-jump-mode . "Dumb")
  :config
  (add-hook 'prog-mode-hook 'dumb-jump-mode))

;; This kinda looks like a dependency to neotree theme
(use-package all-the-icons
  :ensure t)

;; NeoTree could *sometimes* be better than Dired.
(use-package neotree
  :ensure t
  :bind ("<f7>" . neotree-toggle))

;; Git ignore modes, and misc stuff major modes.
(use-package gitignore-mode
  :ensure t)

;; According to Emacs Redux, aggressive-indent-mode rocks
;; Let's give it a try.
;; URL: http://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'c-mode-common-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; hydra
(use-package hydra
  :ensure t)

;; evil is good for navigation, lets give it a key.
(use-package evil)

;; Let's make a mode prefix
(defvar my/mode-toggle-map)
(define-prefix-command 'my/mode-toggle-map)
(global-set-key (kbd "<f6>") 'my/mode-toggle-map)

(define-key my/mode-toggle-map "e" #'evil-mode)
(define-key my/mode-toggle-map "w" #'whitespace-mode)
(define-key my/mode-toggle-map "l" #'nlinum-relative-mode)
(define-key my/mode-toggle-map "c" #'color-identifiers-mode)
(define-key my/mode-toggle-map "v" #'visual-line-mode)
(define-key my/mode-toggle-map "p" #'projectile-mode)
(define-key my/mode-toggle-map "k" #'which-key-mode)


;; My projectile grep was making emacs crash, so 500MB should be more than enough
;; for emacs to work. Got this answer from:
;; https://github.com/bbatsov/projectile/issues/600
(setq gc-cons-threshold 500000000)


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

;; Python programming should be in another file.
(require 'python-programming)

;; Web programming
(require 'web-programming)

;; js programming. God this is annoying
(require 'js-programming)

;; LaTeX writing.
(require 'latex-writing)

;; Mail conf
(require 'setup-mail)

;; News reading conf
(require 'setup-news)

;; IRC config. Using ERC for now
(require 'setup-chat)

;; Vala mode setuo
(require 'vala-mode)

(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (paredit beacon aggressive-indent gitignore-mode all-the-icons neotree ac-html-bootstrap company-web zzz-to-char hydra helm-projectile company-tern tern js2-mode multiple-cursors rainbow-mode rainbow-delimiters emmet-mode web-mode python-django elpy company-irony-c-headers company-irony flycheck-irony irony evil elfeed-goodies ace-link evil-nerd-commenter latex-preview-pane helm-gtags yasnippet yaml-mode which-key visual-fill-column use-package undo-tree smex smart-comment shell-pop projectile powerline nlinum-relative markdown-mode magit helm flycheck expand-region elfeed direx company-statistics company-quickhelp company-flx color-identifiers-mode autopair auctex ace-window)))
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
 )

;; just to suppress warnings.
(provide 'init)
;;; init.el ends here
