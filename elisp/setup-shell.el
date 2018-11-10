;;; setup-shell.el --- Set up shell related stuff.
;;; Commentary:
;;;                This code is mainly a replica of the Howard Abrams' setup.
;;; Code:

(require 'eshell)

(add-hook 'eshell-mode-hook #'company-mode)

;; scroll on input
(setq eshell-scroll-to-bottom-on-input t)

;; don't really know about this one but sounds sensible
(setq eshell-prefer-lisp-functions nil)

(evil-define-minor-mode-key 'insert 'eshell-mode
  (kbd "C-u") 'eshell-kill-input
  (kbd "C-w") 'backward-kill-word
  (kbd "<tab>") 'company-complete)

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

(defun ach--pyvenv-prompt ()
  "Get virtualenv prompt if its running."
  (when (bound-and-true-p venv-current-name)
    (concat "(" venv-current-name ") ")))

(setq eshell-prompt-function
      (lambda ()
        (let* ((directory (split-directory-prompt (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))))
               (parent (car directory))
               (name (cadr directory))
               (branch (or (curr-dir-git-branch-string (eshell/pwd)) ""))
               (venv (or (ach--pyvenv-prompt) "")))

          (if (eq 'dark (frame-parameter nil 'background-mode))
              (concat   ;; Prompt for Dark Themes
               (propertize venv 'face `(:foreground "#8888FF"))
               (propertize parent 'face `(:foreground "#8888FF"))
               (propertize name   'face `(:foreground "#8888FF" :weight bold))
               (propertize branch 'face `(:foreground "green"))
               (propertize " $"   'face `(:weight ultra-bold))
               (propertize " "    'face `(:weight bold)))

            (concat    ;; Prompt for Light Themes
             (propertize venv   'face `(:foreground "#000000"))
             (propertize parent 'face `(:foreground "blue"))
             (propertize name   'face `(:foreground "blue" :weight bold))
             (propertize branch 'face `(:foreground "dark green"))
             (propertize " $"   'face `(:weight ultra-bold))
             (propertize " "    'face `(:weight bold)))))))

(setq eshell-highlight-prompt nil)

;; ---------------------------------- END HA's copy ------------------------------

;; Exec path from shell. Mainly to get PATH out of my shell into
;; eshell and whatnot
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (setenv "SHELL" "/usr/bin/zsh")

  ;; jesus this was horrible to track
  ;; http://chat.stackexchange.com/rooms/30591/discussion-between-tom-hunt-and-random832
  (setenv "COLORTERM" "rxvt"))

;; Struggled until I found
;; http://stackoverflow.com/questions/13763912/emacs-how-to-change-some-colors-in-m-x-shell
(setq ansi-color-names-vector ["#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
(setq ansi-color-map (ansi-color-make-color-map))

(use-package nginx-mode
  :ensure t
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("^nginx.conf\\'"                            . nginx-mode))
  :defer t
  :commands (nginx-mode))

(provide 'setup-shell)
;;; setup-shell.el ends here
