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


(provide 'c-programming)
;;; c-programming.el ends here
