;;; package --- Summary
;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

;; I read somewhere that this should be here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell))))))

;; According to https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; This can reduce emacs init time. I went from 3.0s to 2.5s
(setq gc-cons-threshold 500000000)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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

;; Paradox is quite nice. I love it! Thanks @malabarba!
(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-automatically-star nil)
  (let ((thefile "~/Dropbox/elisp/paradox-token.el"))
    (when (file-exists-p thefile)
      (load-file thefile))))

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

;; Renaming an open buffer file.
(require 'renamefile)

;; File navigation
(require 'setup-file-nav)

;; Need an org mode config file. For now it's small but I'm sure it'll
;; get bigger.
(require 'setup-org)

;; Recentf config.
(require 'setup-recentf)

;; Shell config
(require 'setup-shell)

;; Elisp programming
(require 'elisp-programming)

;; C programming requires a special section
(require 'c-programming)

;; Python programming should be in another file.
(require 'python-programming)

;; Web programming
(require 'web-programming)

;; Ruby programming
(require 'ruby-programming)

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

;; Load UI stuff last
(require 'setup-ui)

;; Ediff
(require 'setup-ediff)

;; Mode line. This should be after everything.
(require 'setup-modeline)

;;; Set garbage collection back to a normal value
;; I hope it doesn't make it hang again..
(setq gc-cons-threshold 128000000)

;; just to suppress warnings.
(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
