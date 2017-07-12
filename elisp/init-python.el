;;; python.el --- Python programming
;;; Commentary:
;;; Code:

(require 'python)

;; Anaconda feels sluggish and ultimately useless.
;; I'll be trying jedi.
(use-package company-jedi
  :ensure t
  :config
  (defun ach-python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'ach-python-mode-hook)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "C-c ?") 'jedi:show-doc))

;; Since linters are more clever than me at formatting code, let's use
;; a tool for that, but let's not be too aggresive, just bind it to a
;; key
(use-package py-yapf
  :ensure t
  :init
  (define-key python-mode-map (kbd "C-c TAB") 'py-yapf-buffer))

;; I was using pyvenv for virtualenvwrappers, but it does not work
;; with eshell, so i had to look something else
(use-package virtualenvwrapper
  :ensure t
  :defer t
  :commands (venv-workon venv-deactivate)
  :init
  (define-key python-mode-map (kbd "C-c C-w") 'venv-workon)
  (define-key python-mode-map (kbd "C-c C-d") 'venv-deactivate)
  (define-key meta-m-map (kbd "w") 'venv-workon)
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs/")
  )

;; They say IPython rocks. I guess it does.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"
        python-shell-buffer-name "IPython"))

;; Let's get rid of the region/buffer keybindings by making -yet
;; again- a dwim command. Bind it to C-c C-c
(defun ach-python-shell-send-dwim (&optional send-main msg)
  "If region is active, send region to buffer, otherwise send the entire buffer.
The SEND-MAIN and MSG arguments are the same as in python-shell-send-region and
python-shell-send-buffer."
  (interactive (list current-prefix-arg t))
  (if (region-active-p)
      (python-shell-send-region (region-beginning)
                                (region-end)
                                send-main
                                msg)
    (progn
      (save-restriction
        (widen)
        (python-shell-send-region (point-min)
                                  (point-max)
                                  send-main
                                  msg)))))

;; While the former function is pretty ok with everything and you
;; don't lose any functionality, it's kind of a bummer that you have
;; to C-c C-p before sending the buffer. Why can't we just C-c C-c and
;; create a Python shell if there isn't one active? Let's just take
;; the magnars approach here: Don't wait for anyone to fix it, just
;; code it and be happy. It kind of sucks that print statements show
;; before completion the first time the command is run, but I'm fine
;; with that. I'd rather not have to hit C-c C-p before C-c. Elpy
;; really got a lot of this right.
(defun python-shell-send-dwim (&optional send-main msg)
  "Send the current region or buffer to shell.

Start the shell if it's not up, SEND-MAIN and MSG are the same
arguments that `ach-python-shell-swnd-dwim' takes."
  (interactive (list current-prefix-arg t))
  (condition-case nil
      (call-interactively 'ach-python-shell-send-dwim)
    (error (progn
             (run-python (python-shell-calculate-command) nil t)
             (call-interactively 'ach-python-shell-send-dwim)))))

;; There's a great approach to the "run project" problem in Spacemacs
;; that I thought is very valuable. Just set a hotkey to run the
;; current file in a Shell buffer. Quite simple, huh?  It also works
;; with pyvenv out of the box. Could this be more wonderful?
(defun spacemacs/python-execute-file (arg)
  "Execute a python script in a shell.

If ARG is present, ask for a command to run."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "python %s" (file-name-nondirectory
                                              buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))


;; Let's add more tweaks to the Python key bindings. I'd like to use
;; C-c C-z (which is python-shell-switch-to-shell) without having to
;; C-c C-p before. If it's not there, well, start it, why wouldn't we
;; do that?
(defun ach-python-switch-to-shell ()
  "Switch to Python shell in other window, create a Python shell if it doesn't exist."
  (interactive)
  (condition-case nil
      (python-shell-switch-to-shell)
    (error (progn
             (run-python)
             (python-shell-switch-to-shell)))))

;; Add tweaks to standard python.el defined in this file.
;; Shell related commands
(define-key python-mode-map (kbd "C-c C-z") #'ach-python-switch-to-shell)
(define-key python-mode-map (kbd "C-c C-r") #'spacemacs/python-execute-file)
(define-key python-mode-map (kbd "C-c C-c") #'python-shell-send-dwim)

;; Navigation
(use-package indent-tools
  :ensure t)

(add-hook 'python-mode-hook (lambda () (require 'indent-tools)))

(with-eval-after-load 'indent-tools
  (define-key python-mode-map (kbd "C-M-n") 'indent-tools-goto-next-sibling)
  (define-key python-mode-map (kbd "C-M-p") 'indent-tools-goto-previous-sibling)
  (define-key python-mode-map (kbd "M-k") 'indent-tools-kill-level)
  (define-key python-mode-map (kbd "C-M-d") 'indent-tools-goto-child)

  ;; Evaluate the possibility to bind this to a key
  (defun ach-raise-current-indent-level ()
    "Raise current indent level."
    (interactive)
    (save-excursion
      (indent-tools-goto-parent)
      (indent-tools-demote)
      (kill-whole-line)
      (move-beginning-of-line 1)
      (newline-and-indent)))

  ;; Promoting and demoting
  (define-key python-mode-map (kbd "C-c ]") 'indent-tools-indent)
  (define-key python-mode-map (kbd "C-c [") 'indent-tools-demote)
  )

(define-key python-mode-map (kbd "C-M-f") #'python-nav-forward-sexp)
(define-key python-mode-map (kbd "C-M-b") #'python-nav-backward-sexp)
(define-key python-mode-map (kbd "C-M-a") #'python-nav-backward-defun)
(define-key python-mode-map (kbd "C-M-e") #'python-nav-end-of-defun)

;; Debugging stuff
(defvar ach-python-debug-string
  "import ipdb; ipdb.set_trace()"
  "Python debugging string to insert in buffers.")

(defun ach--python-insert-debug ()
  "Insert `ach-python-debug-string' in a line before this one."
  (back-to-indentation)
  (insert ach-python-debug-string)
  (newline-and-indent))

(defun ach--python-remove-debug-calls ()
  "Remove every line containing `ach-python-debug-string' in buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((re (concat "^\\s-*" ach-python-debug-string "$")))
        (while (re-search-forward re nil t)
          (kill-whole-line))))))

(defun ach-python-debug (&optional arg)
  "Insert `ach-python-debug-string' in line above, remove all of them with ARG prefix."
  (interactive "P")
  (if arg
      (ach--python-remove-debug-calls)
    (ach--python-insert-debug)))

(define-key python-mode-map (kbd "C-c C-e") 'ach-python-debug)

;; Dealing with imports is a pain in Python
(autoload 'importmagic-mode "importmagic")
(add-hook 'python-mode-hook 'importmagic-mode)
(define-key python-mode-map (kbd "C-c C-f") nil)
(with-eval-after-load 'importmagic
  (define-key importmagic-mode-map (kbd "C-c C-f") 'importmagic-fix-imports)
  (define-key importmagic-mode-map (kbd "C-c C-l") nil)
  (setq importmagic-style-configuration-alist '((multiline . backslash)
                                                (max_columns . 2000)))
  (diminish 'importmagic-mode))

;; Use autoflake to remove unused crap. This idea comes from spacemacs
;; and the following URL
;; https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun python-remove-unused-imports ()
  "Use Autoflake to remove unused imports.

Runs autoflake --remove-all-unused-imports -i file"
  (interactive)
  (shell-command
   (format "autoflake --remove-all-unused-imports -i %s"
           (shell-quote-argument (buffer-file-name))))
  (revert-buffer t t t))

;; Bind it to C-c C-a (mnemonic is [a]utoflake)
(define-key python-mode-map (kbd "C-c C-a") 'python-remove-unused-imports)

;; Ok. Now that we have gotten rid of unused imports, need to sort
;; them. py-isort lets us do that
(use-package py-isort
  :ensure t
  :defer t
  :init
  (define-key python-mode-map (kbd "C-c C-s") 'py-isort-buffer))

;; Another package of mine: projectile-django
(when (file-exists-p "/home/nsalas/forks/projectile-django/projectile-django.el")
  (require 'projectile-django)
  (global-set-key (kbd "M-m d") 'projectile-django-map)
  (setq projectile-django-default-port 8001)
  )

;; Also get rid of the annoying buffers for ivy and helm.
(with-eval-after-load 'helm-buffers
  (add-to-list 'helm-boring-buffer-regexp-list "\\*epc con"))
(with-eval-after-load 'ivy
  (add-to-list 'ivy-ignore-buffers "\\*epc con"))

(add-hook 'python-mode-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook (lambda () (color-identifiers-mode -1)))

;; pip requirements. Looks like a very good package
(use-package pip-requirements
  :ensure t
  :defer t
  :config
  (defun ach--setup-company-pip-requirements ()
    (setq-local company-idle-delay 0.3)
    (setq-local company-backends '(company-capf)))
  (add-hook 'pip-requirements-mode-hook 'ach--setup-company-pip-requirements))

(provide 'init-python)
;;; init-python.el ends here
