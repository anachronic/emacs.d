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
  :bind (("M-x" . counsel-M-x)
         ("C-x f" . counsel-recentf))
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
  (counsel-projectile-mode))

;; I guess this can't hurt
(use-package ivy-rich
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer 'ivy-rich-switch-buffer-transformer))

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
  :config
  (defun ach-company-complete-if-only-one ()
    "Complete candidate if there's only one option, otherwise yas-expand"
    (interactive)
    (if (eq 1 company-candidates-length)
        (company-complete)
      (yas-expand)))
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'comint-mode-hook #'company-mode)
  (define-key company-active-map (kbd "TAB") #'ach-company-complete-if-only-one)
  (define-key company-active-map (kbd "<tab>") #'ach-company-complete-if-only-one)
  (define-key company-active-map (kbd "C-j") 'company-complete-common-or-cycle)

  ;; Default settings
  (setq company-idle-delay nil
        company-tooltip-align-annotations t)

  ;; default backends
  (setq-default company-backends
                '(company-bbdb
                  company-nxml company-css
                  company-eclim company-semantic company-clang
                  company-xcode company-cmake
                  company-capf
                  company-files
                  (company-dabbrev-code company-gtags company-etags
                                        company-keywords)
                  company-oddmuse))

  ;; The following comes from
  ;; https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387
  ;; This means no idle completion and no annoying C-S-SPC
  (define-key company-mode-map [remap indent-for-tab-command]
    'company-indent-for-tab-command)

  (setq tab-always-indent 'complete)

  (defvar completion-at-point-functions-saved nil)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))
  :diminish "c"
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
  :after flx
  :config
  (company-flx-mode +1)
  (setq company-flx-limit 75))

;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-/") 'hippie-expand)

;; Hippie expand can be very annoying with list expanding and
;; abbrevs. Config taken from purcell's
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

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
