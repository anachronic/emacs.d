;;; setup-shell.el --- Set up shell related stuff.
;;; Commentary:
;;;                This code is mainly a replica of the Howard Abrams' setup.
;;; Code:

;; Popping an eshell is pretty useful
(use-package shell-pop
  :ensure t
  :demand
  :config
  (define-key meta-m-map (kbd "M-z") 'shell-pop))

(require 'eshell)

;; Need to set the same value for exec-path and eshell-path-env
(add-hook 'eshell-mode-hook (lambda ()
                              (let ((nsv/userpath (getenv "PATH")))
                                (setq eshell-path-env nsv/userpath)
                                (setq exec-path (s-split ":" nsv/userpath)))))

(add-hook 'eshell-mode-hook #'company-mode)

;; scroll on input
(setq eshell-scroll-to-bottom-on-input t)

;; don't really know about this one but sounds sensible
(setq eshell-prefer-lisp-functions nil)


;; This part is entirely copied from Howard Abrams' config.
;; I'll make a reference in the README.md file.
(defun curr-dir-git-branch-string (pwd)
  "Return current git branch as a string, or the empty string if PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (if (> (length git-output) 0)
          (concat " :" (substring git-output 0 -1))
        "(no branch)"))))

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                               (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd  ;; Otherwise, we just return the PWD
      )))

;; Turn off the default prompt.
(setq eshell-highlight-prompt nil)

(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(setq eshell-prompt-function
      (lambda ()
        (let* ((directory (split-directory-prompt (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))))
               (parent (car directory))
               (name (cadr directory))
               (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")))

          (if (eq 'dark (frame-parameter nil 'background-mode))
              (concat   ;; Prompt for Dark Themes
               (propertize parent 'face `(:foreground "#8888FF"))
               (propertize name   'face `(:foreground "#8888FF" :weight bold))
               (propertize branch 'face `(:foreground "green"))
               (propertize " $"   'face `(:weight ultra-bold))
               (propertize " "    'face `(:weight bold)))

            (concat    ;; Prompt for Light Themes
             (propertize parent 'face `(:foreground "blue"))
             (propertize name   'face `(:foreground "blue" :weight bold))
             (propertize branch 'face `(:foreground "dark green"))
             (propertize " $"   'face `(:weight ultra-bold))
             (propertize " "    'face `(:weight bold)))))))

(setq eshell-highlight-prompt nil)

;; ---------------------------------- END HA's copy ------------------------------
;; well, aliases go in eshell/ directory, so i won't bother with that for this.

;; I use C-d quite frequently when in shells. I guess I want that
;; too. Thank you Howard!
(defun ha/eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much))
    (delete-forward-char arg)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-d")
              'ha/eshell-quit-or-delete-char)))

;; Sometimes a shell is better...
(define-key meta-m-map (kbd "s") #'shell)

;; Need color, mainly for python and shell coloring
(use-package xterm-color
  :ensure t
  :config
  ;; eshell color
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  ;; (setq eshell-output-filter-functions '())

  ;; comint color
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter));; (setq comint-output-filter-functions '()))

;; Exec path from shell. Mainly to get PATH out of my shell into
;; eshell and whatnot
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (setenv "SHELL" "/usr/bin/zsh")
  (setenv "TERM" "xterm-256color"))

(provide 'setup-shell)
;;; setup-shell.el ends here
