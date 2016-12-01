;;; package --- Summary
;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

;; According to https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; This can reduce emacs init time. I went from 3.0s to 2.5s
(setq gc-cons-threshold 500000000)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; For some reason I find it hard to navigate package list without
;; highlighting the line.
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; I DO NOT LIKE TYPING YES!!!!
(fset 'yes-or-no-p 'y-or-n-p)

;; General paths. Self-written scripts and themes.
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp/")))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "site-lisp/")))
(add-to-list 'custom-theme-load-path (expand-file-name (concat user-emacs-directory "themes/")))

;; Nobody likes those annoying ~ end files, so redirect them.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Base auto-install and pretty code for configuring packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Emacs core stuff
(require 'setup-emacs)

;; I'd like to have a cool prefix key to do my own things without
;; having to worry that anyone overrides its key binding and, as you
;; know, C-c is quite cluttered. So, let's rebind C-a to combine M-m
;; and C-a and voilà!, we get a free cool key to bind to a prefix.
;; The rebinding happens in setup-editor.el
(global-unset-key (kbd "M-m"))
(define-prefix-command 'meta-m-map)
(global-set-key (kbd "M-m") 'meta-m-map)

;; Add stuff to text so you get visual aid when coding
(require 'setup-editor)

;; Searching and jumping around
(require 'setup-search)

;; This has grown beyond the point of mantainable in init.el
(require 'setup-completions)

;; Version control stuff
(require 'setup-vc)

;; Help enhancement
(require 'setup-help)

;; Buffer moving/killing defuns
(require 'setup-buffers)

;; Visual configuration. Here you'll find stuff about how Emacs looks
(require 'setup-ui)

;; Renaming an open buffer file.
(require 'renamefile)

;; File navigation
(require 'setup-file-nav)

;; Need an org mode config file. For now it's small but I'm sure it'll
;; get bigger.
(require 'setup-org)

;; Recentf config.
(require 'setup-recentf)

;; Eshell config.
(require 'setup-eshell)

;; Elisp programming
(require 'elisp-programming)

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

;; Markdown writing
(require 'markdown-writing)

;; Misc modes that don't need a separate file: (yaml, markdown)
(require 'setup-modes)

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
    (indent-guide notmuch engine-mode page-break-lines camcorder markdown-preview-mode epc link-hint diff-hl dired+ ibuffer-vc key-chord restart-emacs help-fns+ smex immortal-scratch hl-sexp highlight-symbol highlight-quoted anzu iy-go-to-char command-log-mode pyvenv py-yapf company-anaconda anaconda-mode org-plus-contrib gist browse-at-remote lorem-ipsum fullframe htmlize ox-reveal paredit beacon aggressive-indent gitignore-mode neotree ac-html-bootstrap company-web zzz-to-char hydra helm-projectile company-tern tern js2-mode multiple-cursors rainbow-mode rainbow-delimiters emmet-mode web-mode python-django elpy company-irony-c-headers company-irony flycheck-irony irony evil elfeed-goodies ace-link evil-nerd-commenter latex-preview-pane helm-gtags yasnippet yaml-mode which-key visual-fill-column use-package undo-tree smart-comment shell-pop projectile powerline nlinum-relative markdown-mode magit helm flycheck expand-region elfeed direx company-statistics company-quickhelp company-flx color-identifiers-mode autopair auctex ace-window)))
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (file-remote-p default-directory)
         " Projectile"
       (format " [%s]"
               (projectile-project-name))))))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "<f5>")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Set garbage collection back to a normal value
;; I hope it doesn't make it hang again..
(setq gc-cons-threshold 128000000)

;; just to suppress warnings.
(provide 'init)
;;; init.el ends here
