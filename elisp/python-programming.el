;;; python.el --- Python programming
;;; Commentary:
;;; Code:

;; Switch to anaconda-mode from Elpy
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook #'anaconda-mode))

;; Use company anaconda for completions. capf seems REALLY slow
(use-package company-anaconda
  :ensure t
  :after anaconda-mode
  :config
  (defun my/add-company-anaconda ()
    (setq-local company-backends company-backends)
    (add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook #'my/add-company-anaconda))

;; Since linters are more clever than me at formatting code, let's use
;; a tool for that, but let's not be too aggresive, just bind it to a
;; key
(use-package py-yapf
  :ensure t
  :config
  (defun my/redef-indent-python ()
    (local-set-key (kbd "C-c TAB") #'py-yapf-buffer))
  (add-hook 'python-mode-hook #'my/redef-indent-python))

;; I liked the pyvenv tool from Elpy. So let's use that. Also, I
;; pretty much use (and will continue to do so) virtualenvwrapper.sh,
;; Thus, the binding that will be used is pyvenv-workon in C-c C-w. We
;; will also need a deactivate key, C-c C-d makes sense.
(use-package pyvenv
  :ensure t
  :config
  (defun my/set-pyvenv-workon ()
    (local-set-key (kbd "C-c C-w") #'pyvenv-workon)
    (local-set-key (kbd "C-c C-d") #'pyvenv-deactivate))
  (add-hook 'python-mode-hook #'my/set-pyvenv-workon))

;; The warning message is very annoying, let's get rid of it
;; Solution found at:
;; https://github.com/jorgenschaefer/elpy/issues/887#issuecomment-261696405
(defun python-shell-completion-native-try ()
  "Return non-nil if can trigger native completion."
  (let ((python-shell-completion-native-enable t)
        (python-shell-completion-native-output-timeout
         python-shell-completion-native-try-output-timeout))
    (python-shell-completion-native-get-completions
     (get-buffer-process (current-buffer))
     nil "_")))

;; They say IPython rocks. I guess it does.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

;; Let's get rid of the region/buffer keybindings by making -yet
;; again- a dwim command. Bind it to C-c C-c
(defun python-shell-send-dwim (&optional send-main msg)
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

(defun my/replace-python-send-buffer ()
  "Replace python-shell-send-buffer key binding to python-shell-send-dwim."
  (local-set-key (kbd "C-c C-c") #'python-shell-send-dwim))

(add-hook 'python-mode-hook #'my/replace-python-send-buffer)


;; There's a great approach to the "run project" problem in Spacemacs
;; that I thought is very valuable. Just set a hotkey to run the
;; current file in a Shell buffer. Quite simple, huh?  It also works
;; with pyvenv out of the box. Could this be more wonderful?
(defun spacemacs/python-execute-file (arg)
  "Execute a python script in a shell."
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


;; The ideal binding for this is C-c C-r, which is unfortunately bound
;; to python-shell-send-region. Since that's kind of a stupid matter,
;; I should code a function python-shell-send-region-or-buffer-dwim
;; which could be bound to C-c C-c and then I could use C-c C-r for
;; "running" the project.
(defun my/add-python-execute ()
  "Add a Python execute file key binding to the buffer."
  (local-set-key (kbd "C-c C-r") #'spacemacs/python-execute-file))

;; Well, I guess I fixed the C-c C-c issue. So let's use an
;; appropriate key binding for running the file
(add-hook 'python-mode-hook #'my/add-python-execute)

;; python-django seems to work quite ok with it.
(use-package python-django
  :ensure t)

;; For some reason color identifiers mode doesn't start with Python I
;; guess either Python is not a prog-mode or Elpy doesn't play nice
;; with it. Whatever
(defun my/add-color-identifiers-mode ()
  "Add color identifiers mode to the buffer or whatever."
  (color-identifiers-mode 1))
(add-hook 'python-mode-hook #'my/add-color-identifiers-mode)

(provide 'python-programming)
;;; python-programming.el ends here
