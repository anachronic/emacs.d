;;; c-programming.el --- Relevant stuff for coding in C.
;;; Commentary:
;;; Code:

;; We do love GTAGS.

;; I hate abbrev when coding c-like languages!!
(defun nsv/abbrev-gtfo ()
  "Get rid of abbrev."
  (abbrev-mode -1))

(add-hook 'c-mode-common-hook 'nsv/abbrev-gtfo)

;; I'll set the default style to k&r.
;; we might want to try out linux. We'll see
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))


;; Will be trying irony mode for FlyC and company
(use-package irony
  :ensure t
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; auto-install the server for irony mode.
(when (not (file-exists-p "~/.emacs.d/irony"))
  (irony-install-server (format
                         (concat "%s %s %s && %s --build . "
                                 "--use-stderr --config Release --target install")
                         (shell-quote-argument irony-cmake-executable)
                         (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                                       (expand-file-name
                                                        irony-server-install-prefix)))
                         (shell-quote-argument irony-server-source-dir)
                         (shell-quote-argument irony-cmake-executable))))

;; We want to use flycheck with irony.
(use-package flycheck-irony
  :ensure t)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Add company-irony and company-irony-c-headers to company backends
(use-package company-irony
  :ensure t)

(use-package company-irony-c-headers
  :ensure t)


(defun nsv/add-irony-to-company ()
  "Setup company to work with irony mode."
  (setq-local company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends '(company-irony company-irony-c-headers)))

(add-hook 'c-mode-hook 'nsv/add-irony-to-company)
(add-hook 'c++-mode-hook 'nsv/add-irony-to-company)


(defun nsv/add-header-jump ()
  "Add a header jump key binding."
  (local-set-key (kbd "C-c j h") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'nsv/add-header-jump)

(provide 'c-programming)
;;; c-programming.el ends here
