;;; python.el --- Python programming
;;; Commentary:
;;; Code:

(require 'python)

;; Anaconda feels sluggish and ultimately useless.
;; I'll be trying jedi.
(use-package company-jedi
  :ensure t
  :config
  (defun nsv/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'nsv/python-mode-hook)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "C-c ?") 'jedi:show-doc))

;; Since linters are more clever than me at formatting code, let's use
;; a tool for that, but let's not be too aggresive, just bind it to a
;; key
(use-package py-yapf
  :ensure t
  :config
  (defun nsv/redef-indent-python ()
    (local-set-key (kbd "C-c TAB") #'py-yapf-buffer))
  (add-hook 'python-mode-hook #'nsv/redef-indent-python))

;; I liked the pyvenv tool from Elpy. So let's use that. Also, I
;; pretty much use (and will continue to do so) virtualenvwrapper.sh,
;; Thus, the binding that will be used is pyvenv-workon in C-c C-w. We
;; will also need a deactivate key, C-c C-d makes sense.
(use-package pyvenv
  :ensure t
  :config
  (defun nsv/set-pyvenv-workon ()
    (local-set-key (kbd "C-c C-d") #'pyvenv-deactivate)
    (local-set-key (kbd "C-c C-w") #'pyvenv-workon))
  (add-hook 'python-mode-hook #'nsv/set-pyvenv-workon))

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
        python-shell-interpreter-args "--simple-prompt -i"
        python-shell-buffer-name "IPython"))

;; Let's get rid of the region/buffer keybindings by making -yet
;; again- a dwim command. Bind it to C-c C-c
(defun nsv/python-shell-send-dwim (&optional send-main msg)
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
  (interactive (list current-prefix-arg t))
  (condition-case nil
      (call-interactively 'nsv/python-shell-send-dwim)
    (error (progn
             (run-python (python-shell-calculate-command) nil t)
             (call-interactively 'nsv/python-shell-send-dwim)))))

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


;; Let's add more tweaks to the Python key bindings. I'd like to use
;; C-c C-z (which is python-shell-switch-to-shell) without having to
;; C-c C-p before. If it's not there, well, start it, why wouldn't we
;; do that?
(defun nsv/python-switch-to-shell ()
  "Switch to Python shell in other window, create a Python shell if it doesn't exist."
  (interactive)
  (condition-case nil
      (python-shell-switch-to-shell)
    (error (progn
             (run-python)
             (python-shell-switch-to-shell)))))

;; Default yanking in python sucks ballz. I had to rewrite my own
;; function for it. It's really hard to infer how one should do the
;; indentation. This functions assumes that you hit C-y at the level
;; you want.
(defun nsv/python-yank (arg)
  "Yank and reindent python code, with ARG, dont reindent."
  (interactive "P")
  (yank)
  (when arg
    (save-excursion
      (exchange-point-and-mark)
      (forward-line 1)
      (let ((times (count-lines (region-beginning) (region-end))))
        (deactivate-mark)
        (dotimes (i times)
          (indent-for-tab-command)
          (when (< i times)
            (forward-line 1)))))))


;; Add tweaks to standard python.el defined in this file.
(defun nsv/python-rebinds ()
  "Add the functions defined in python-programming.el to Python buffer locally."
  ;; Basic commands
  (define-key python-mode-map (kbd "C-y") #'nsv/python-yank)
  ;; Shell related commands
  (local-set-key (kbd "C-c C-z") #'nsv/python-switch-to-shell)
  (local-set-key (kbd "C-c C-r") #'spacemacs/python-execute-file)
  (local-set-key (kbd "C-c C-c") #'python-shell-send-dwim)
  ;; I don't know why these are not defaults
  (local-set-key (kbd "C-M-f") #'python-nav-forward-sexp)
  (local-set-key (kbd "C-M-b") #'python-nav-backward-sexp)
  (local-set-key (kbd "C-M-a") #'python-nav-backward-defun)
  (local-set-key (kbd "C-M-e") #'python-nav-forward-defun))

(add-hook 'python-mode-hook #'nsv/python-rebinds)

;; Debugging
(defun nsv/python-debug ()
  "Insert import ipdb; ipdb.set_trace() in the buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()")
  (newline-and-indent))

(define-key python-mode-map (kbd "C-c C-e") 'nsv/python-debug)

;; Try out fakegir
(when (file-exists-p "~/.cache/fakegir")
  (add-to-list 'python-shell-extra-pythonpaths "~/.cache/fakegir"))

;; python-django seems to work quite ok with it.
(use-package python-django
  :ensure t)

;; Trying out my new package
(when (file-exists-p "/home/nsalas/forks/importmagic.el")
  (require 'importmagic)
  (add-hook 'python-mode-hook 'importmagic-mode)
  (define-key python-mode-map (kbd "C-c C-f") nil)
  (define-key importmagic-mode-map (kbd "C-c C-f") 'importmagic-fix-imports))

;; Diminish it to free up mode line space.
(diminish 'importmagic-mode)

;; Also get rid of the annoying buffers for ivy and helm.
(with-eval-after-load 'helm
  (add-to-list 'helm-boring-buffer-regexp-list "\\*epc con"))
(with-eval-after-load 'ivy
  (add-to-list 'ivy-ignore-buffers "\\*epc con"))

;; Python needs indentation, Emacs should help
(add-hook 'python-mode-hook #'indent-guide-mode)

(with-eval-after-load 'anaconda-mode
  (define-key anaconda-mode-map (kbd "C-M-g") 'anaconda-mode-find-definitions)
  (define-key anaconda-mode-map (kbd "C-M-p") 'anaconda-mode-go-back)
  (define-key anaconda-mode-map (kbd "M-.") nil)
  (define-key anaconda-mode-map (kbd "M-,") nil)
  (define-key python-mode-map (kbd "M-.") 'dumb-jump-go)
  (define-key python-mode-map (kbd "M-,") 'dumb-jump-back))

;; Been using paredit in this thing
(define-key python-mode-map (kbd "C-)") #'paredit-forward-slurp-sexp)

(add-hook 'python-mode-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook (lambda () (color-identifiers-mode -1)))

(provide 'python-programming)
;;; python-programming.el ends here
