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

  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs/")
  )

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
