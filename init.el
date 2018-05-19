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
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (melpaurl (concat (if no-ssl "http" "https") "://melpa.org/packages/"))
       (orgurl (concat "https" "://orgmode.org/elpa/")))
  (add-to-list 'package-archives (cons "melpa" melpaurl) t))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Emacs community is awesome
;; https://www.reddit.com/r/emacs/comments/6s9ez3/package_management_under_emacs_25/dlbzvyc/
(setq package-archive-priorities
      '(("org"          . 200)
        ("melpa"        . 100)
        ("gnu"          .  50)))

;; I DO NOT LIKE TYPING YES!!!!
(fset 'yes-or-no-p 'y-or-n-p)

;; General paths. Self-written scripts and themes.
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp/")))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "site-lisp/")))
(add-to-list 'custom-theme-load-path (expand-file-name (concat user-emacs-directory "themes/")))

;; Nobody likes those annoying ~ end files, so redirect them.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Base auto-install and pretty code for configuring packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; We'll be using evil
(require 'setup-evil)

;; Some evil fixes. Mainly from evil-collection.el
(require 'setup-evil-fixes)

;; Emacs core stuff
(require 'setup-emacs)

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

;; File navigation
(require 'setup-file-nav)

;; Need an org mode config file. For now it's small but I'm sure it'll
;; get bigger.
(require 'setup-org)

;; Gnus
(require 'setup-gnus)

;; Shell config
(require 'setup-shell)

;; Elisp programming
(require 'init-elisp)

;; C programming requires a special section
(require 'init-c)

;; Python programming should be in another file.
(require 'init-python)

;; Web programming
(require 'init-web-mode)

;; Ruby programming
(require 'init-ruby)

;; js programming. God this is annoying
(require 'init-javascript)

;; LaTeX writing.
(require 'init-latex)

;; Markdown writing
(require 'init-markdown)

;; Misc modes that don't need a separate file: (yaml, markdown)
(require 'setup-modes)

;; More GUI stuff: images, pdfs, svgs, etc..
(require 'setup-docviews)

;; Load UI stuff last
(require 'setup-ui)

;; Ediff
(require 'setup-ediff)

;; Blogging
(require 'setup-blogging)

;; Mode line. This should be after everything.
(require 'setup-modeline)

;; Databases
(require 'init-databases)

(maybe-load-file "~/Dropbox/elisp/personal-definitions.el")

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
