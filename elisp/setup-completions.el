;;; setup-ivy-helm-completions.el --- General completion system.
;;; Commentary:
;;; Code:

;;; This file handles every completion system my config has. That
;;; means: minibuffer completions, file search, grep, autocompletion
;;; when coding and snippets. It also includes every package related
;;; to them, like company enhancements, for instance.

;; Smex provides a GREAT interface for M-x. I got rid of it at some
;; point but i really regret it, mostly for the last used commands.
(use-package smex
  :ensure t)

;; I've really gotten into Ivy. I think I'll be dropping helm
;; soon. Especially because I managed to make ivy behave the way I
;; wanted with tab and C-l
(use-package ivy
  :ensure t
  :demand
  :diminish 'ivy-mode
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-git-grep . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist
        '((counsel-M-x . "^")
          (man . "^")
          (woman . "^")))
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 16)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done)
  (define-key ivy-minibuffer-map [tab] #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'keyboard-escape-quit)
  (ivy-mode 1))

;; ivy-hydra is not too horrible
(use-package ivy-hydra
  :ensure t
  :demand)

;; I've come to think helm is not really good with files anymore. So let's
;; use counsel. It also has some nice builtin functionality:
;; http://oremacs.com/2015/04/19/git-grep-ivy/
(use-package counsel
  :ensure t
  :demand
  :bind (("M-x" . counsel-M-x))
  :config

  ;; For some reason evil hijacks C-.
  (define-key evil-normal-state-map (kbd "C-.") 'counsel-imenu))

;; I had a mix of stuff before. So it's nice to decide on
;; counsel-projectile after all. I don't even projectile that much,
;; but using counsel makes it way more consistent than it was before
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (evil-define-key 'normal global-map (kbd "C-p") 'counsel-projectile-find-file)
  (evil-leader/set-key
    "a" 'counsel-projectile-rg)
  (counsel-projectile-mode))

;; I guess this can't hurt
(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

;; YASnippet, always so handy...
(use-package yasnippet
  :ensure t
  :diminish 'yas-minor-mode
  :config
  (setq-default yas-snippet-dirs `(,(expand-file-name
                                     (concat user-emacs-directory
                                             "snippets"))))
  (yas-global-mode 1))

;; I do like an autocompletion system, even though this really has to
;; mature in Emacs.
(use-package company
  :ensure t
  :demand
  :init
  (global-company-mode)
  (defun ach-add-company-backend-locally (symbol)
    (setq-local company-backends company-backends)
    (add-to-list 'company-backends symbol))
  :diminish "C"
  :bind (("C-S-<SPC>" . company-complete)))

;; Help is cool
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;; flx is a dependency for company-flx and ivy's M-x (or any ivy
;; actually).
(use-package flx
  :demand
  :ensure t)

;; Will be trying company-flx for a while.
(use-package company-flx
  :ensure t
  :defer t
  :config
  (setq company-flx-limit 75))

;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Hippie expand can be very annoying with list expanding and
;; abbrevs. Config taken from purcell's
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-file-name-partially))

;; I saw this talk and it blew my mind about some vim completions.
;; https://youtu.be/3TX3kV3TICU
;; Hippie expand can do something similar
;; Source: https://stackoverflow.com/a/17928654/2066658
(defun expand-line ()
  "Expand current line using `hippie-expand'."
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))

(define-key ctl-x-map (kbd "C-l") 'expand-line)

(provide 'setup-completions)
;;; setup-completions.el ends here
