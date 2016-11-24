;;; c-programming.el --- Relevant stuff for coding in C.
;;; Commentary:
;;; Code:

;; We do love GTAGS.
;; I couldn't get the helm-gtags prefix. But that's ok
(if (executable-find "global")
    (use-package helm-gtags
      :ensure t
      :diminish "TAGS"
      :config
      (progn
        (setq
         helm-gtags-ignore-case t
         helm-gtags-auto-update t)
        (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
        (define-key helm-gtags-mode-map (kbd "C-t .") 'helm-gtags-dwim)
        (define-key helm-gtags-mode-map (kbd "C-t C-s") 'helm-gtags-find-symbol)
        (define-key helm-gtags-mode-map (kbd "C-t C-f") 'helm-gtags-find-files)
        (define-key helm-gtags-mode-map (kbd "C-t C-u") 'helm-gtags-update-tags)
        (define-key helm-gtags-mode-map (kbd "C-t c") 'helm-gtags-create-tags)
        (define-key helm-gtags-mode-map (kbd "C-t f") 'helm-gtags-find-tag)
        (define-key helm-gtags-mode-map (kbd "C-t p") 'helm-gtags-find-pattern)
        (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
  (message "c-programming.el: GNU GLOBAL not found in PATH. helm-gtags will not be working."))

(add-hook 'c-mode-common-hook 'helm-gtags-mode)

;; I hate abbrev when coding c-like languages!!
(defun my/abbrev-gtfo ()
  "Get rid of abbrev."
  (abbrev-mode -1))

(add-hook 'c-mode-common-hook 'my/abbrev-gtfo)

;; I'll set the default style to k&r.
;; we might want to try out linux. We'll see
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))


;; Will be trying irony mode for FlyC and company
(use-package irony
  :ensure t
  :pin melpa-stable
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


(defun my/add-irony-to-company ()
  "Setup company to work with irony mode."
  (setq-local company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends '(company-irony company-irony-c-headers)))

(add-hook 'c-mode-hook 'my/add-irony-to-company)
(add-hook 'c++-mode-hook 'my/add-irony-to-company)


(defun my/add-header-jump ()
  "Add a header jump key binding."
  (local-set-key (kbd "C-c j h") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'my/add-header-jump)


;; Key chords for C/C++
(require 'key-chord)

;; Put every key chord for C mode into this function
(defun my/add-keychords-c-mode ()
  "Add ;; keystroke to mean insert semicolon at the end of the line."
  (key-chord-define-local ";;" "\C-e;"))

(add-hook 'c-mode-common-hook 'my/add-keychords-c-mode)


(provide 'c-programming)
;;; c-programming.el ends here
