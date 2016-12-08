;;; setup-eshell.el --- Set up eshell related stuff.
;;; Commentary:
;;;                This code is mainly a replica of the Howard Abrams' setup.
;;; Code:

;; I don't mind putting this in here since I do use shell-pop with
;; eshell
(use-package shell-pop
  :ensure t
  :demand
  :config
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
  (setq shell-pop-universal-key "M-m M-z"))


;; Set the PATH. Should determine between bash and zsh configs atm.
(require 's)
(setenv "PATH"
        (let ((thepath (getenv "PATH"))
              (thefile (cond ((file-exists-p "~/.zshrc") "~/.zshrc")
                             ((file-exists-p "~/.bashrc") "~/.bashrc")
                             (t ""))))
          (if (s-equals? thefile "")
              thepath
            (let ((thestring
                   (s-chomp (shell-command-to-string
                             (concat "cat "
                                     thefile
                                     " | grep \"^[^#\]\" | grep \"export PATH\"")))))
              (concat (s-chop-prefix "export PATH=" (s-chop-suffix "$PATH" thestring))
                      (getenv "PATH"))))))

;; Need to set the same value for exec-path and eshell-path-env
(add-hook 'eshell-mode-hook (lambda ()
                              (let ((my/userpath (getenv "PATH")))
                                (setq eshell-path-env my/userpath)
                                (setq exec-path (s-split ":" my/userpath)))))


;; scroll on input
(setq eshell-scroll-to-bottom-on-input t)

;; don't really know about this one but sounds sensible
(setq eshell-prefer-lisp-functions nil)


;; This part is entirely copied from Howard Abrams' config.
;; I'll make a reference in the README.md file.
(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if PWD is not in a git repo (or the git command is not found)."
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
        (eshell-life-is-too-much) ; Why not? (eshell/exit)
        (delete-window))
    (delete-forward-char arg)))

(add-hook 'eshell-mode-hook (lambda ()
                              (define-key eshell-mode-map (kbd "C-d")
                                'ha/eshell-quit-or-delete-char)))

;; Sometimes a shell is better...
(define-key meta-m-map (kbd "s") #'shell)

(provide 'setup-eshell)
;;; setup-eshell.el ends here
