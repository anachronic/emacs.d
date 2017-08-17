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
 '(package-selected-packages
   (quote
    (spacemacs-theme zenburn-theme tide evil virtualenvwrapper rjsx-mode hexo edbi define-word eros pip-requirements xref-js2 smart-mode-line nginx-mode hungry-delete-mode bm indent-tools py-isort yasnippet yari yaml-mode xterm-color window-numbering whole-line-or-region which-key wgrep web-mode-edit-element web-beautify visual-fill-column use-package undo-tree tagedit switch-window sunshine spaceline smex shell-pop ruby-end restclient restart-emacs rainbow-mode rainbow-delimiters pyvenv pydoc py-yapf persp-projectile paredit-everywhere paradox page-break-lines ox-reveal ox-gfm origami org-plus-contrib org-gcal org-bullets notmuch nlinum-relative neotree multiple-cursors markdown-preview-mode magithub lorem-ipsum link-hint latex-preview-pane key-chord json-reformat js2-mode js-comint iy-go-to-char ivy-rich ivy-hydra indent-guide immortal-scratch ibuffer-vc htmlize hl-sexp highlight-symbol highlight-quoted help-fns+ helm-gtags gmail2bbdb gitignore-mode gist fullframe expand-region exec-path-from-shell evil-nerd-commenter emmet-mode elfeed dumb-jump direx dired-narrow dired-k dired+ diff-hl counsel-projectile company-web company-tern company-statistics company-quickhelp company-jedi company-flx company-bibtex company-anaconda command-log-mode color-identifiers-mode browse-at-remote better-shell beacon bbdb auctex anzu ace-window ac-html-bootstrap)))
 '(safe-local-variable-values (quote ((eval (quote (js2-jsx-mode))) (js2-jsx-mode))))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell))))))

;; According to https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; This can reduce emacs init time. I went from 3.0s to 2.5s
(setq gc-cons-threshold 500000000)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (melpaurl (concat (if no-ssl "http" "https") "://melpa.org/packages/"))
       (orgurl (concat "https" "://orgmode.org/elpa/")))
  (add-to-list 'package-archives (cons "melpa" melpaurl) t))

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
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Base auto-install and pretty code for configuring packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun maybe-install-packages (&rest packages)
  "Install all packages defined in PACKAGES."
  (when packages
    (dolist (pkg packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))))

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

;; Custom keymaps
(require 'setup-keymaps)

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

;; Mail conf
(require 'setup-mail)

;; News reading conf
(require 'setup-news)

;; IRC config. Using ERC for now
(require 'setup-chat)

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
