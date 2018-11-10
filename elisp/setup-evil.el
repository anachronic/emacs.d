;;; setup-evil.el --- Evil mode.
;;; Commentary:
;;; Code:

;; All evil variables that are going to be called by evil should go
;; here since evil already is loaded when the evil-leader use-package
;; sexp finishes interpreting
(setq-default evil-want-C-u-scroll t)
(setq-default evil-want-C-d-scroll t)
(setq-default evil-want-Y-yank-to-eol t)
(setq-default evil-search-module 'evil-search)

(use-package evil-leader
  :demand t
  :ensure t
  :commands (global-evil-leader-mode))

(use-package evil
  :commands (evil-mode evil-define-key)
  :ensure t
  :demand t
  :init
  (global-evil-leader-mode)
  :config
  ;; Leader stuff
  (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode"))
  (evil-leader/set-leader "<SPC>")

  ;; Use visual lines for navigation only
  (define-key evil-normal-state-map "j" 'evil-next-visual-line)
  (define-key evil-normal-state-map "k" 'evil-previous-visual-line)

  ;; Moving around with windows
  (global-unset-key (kbd "C-h"))
  (global-unset-key (kbd "C-j"))
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "C-l"))
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; leader defines
  (evil-leader/set-key
    "<SPC>" 'ivy-switch-buffer
    "RET" 'evil-ex-nohighlight

    ;; buffers
    "k" 'kill-this-buffer
    "w" 'save-buffer
    "br" 'rename-buffer
    "bb" 'counsel-recentf
    "bs" 'save-buffer
    "bS" 'sudo-save
    "bR" 'rename-file-and-buffer ;; defined in setup-buffers.el
    "bk" 'kill-this-buffer

    ;; Files
    "ff" 'find-file

    ;; Dired
    "gj" 'dired-jump

    ;; Only
    "o" 'delete-other-windows

    ;; Eshell
    "s" 'eshell

    ;; Bury buffer
    "q" 'bury-buffer

    ;; "Applications"
    "gan" 'gnus
    "gam" 'mu4e
    )
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(provide 'setup-evil)
;;; setup-evil.el ends here
