;;; setup-editor.el --- Set up everything associated with buffer manipulation.
;;; Commentary:
;;; Code:

;; Visual fill column for text-only buffers, like mail and feeds. and maybe org..
(use-package visual-fill-column
  :ensure t)

;; hydra
(use-package hydra
  :ensure t)

(show-paren-mode 1)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Highlight line. Just add hooks since hl-line is autoloaded
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)

;; Flycheck. What's an editor without error checking?
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-global-modes '(not org-mode comint-mode text-mode)))

;; This is a beautiful command from endlessparentheses
;; url: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first, narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; Let's bind it to C-x n
(define-key ctl-x-map (kbd "n") #'narrow-or-widen-dwim)

;; Remove any trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Line numbers
(if (not (boundp 'display-line-numbers))
    (use-package nlinum-relative
      :ensure t
      :config
      (nlinum-relative-setup-evil)
      (setq nlinum-relative-current-symbol "")
      (setq nlinum-relative-redisplay-delay 0.2))
  (defun ach/set-line-numbers ()
    (setq-default display-line-numbers 'visual))
  (add-hook 'prog-mode-hook 'ach/set-line-numbers)
  (add-hook 'prog-mode-hook 'ach/set-line-numbers))

;; expand region. An *excellent* tool.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Undo-Tree for real undo/redo commands
;; Also required by evil
(use-package undo-tree
  :ensure t
  :diminish ""
  :init
  (global-undo-tree-mode 1)
  :config
  (define-key undo-tree-map (kbd "C-/") nil))

;; Same as in vim: lightweight and great
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; diminish visual-line-mode
(diminish 'visual-line-mode)
(add-hook 'Man-mode-hook 'visual-line-mode)

;; Might need to change this
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

;; According to: https://github.com/joaotavora/autopair,
;; electric-pair-mode is *better* than autopair
(electric-pair-mode)

;; Indent the whole buffer
(defun ach-indent-whole-buffer ()
  "Indent the whole buffer according to the defined style."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "C-c TAB") 'ach-indent-whole-buffer)

;; Author's URL: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;; With this we can remap M-m to something else. We'll see
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Needs some testing
(evil-redirect-digit-argument evil-motion-state-map "0" 'smarter-move-beginning-of-line)

;; and unbind M-m
(global-unset-key (kbd "M-m"))

;; My version of transpose lines. While emacs' transpose lines does the job,
;; I like the IntelliJ/Pycharm implementation better. It's cleaner. So let's
;; do that

(defun ach-move-line-up ()
  "Transpose the current line with the one above leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-beginning-of-line 1)
    (forward-char col)))


(defun ach-move-line-down ()
  "Transpose the current line with the one below leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-beginning-of-line 1)
    (forward-char col)))

(global-set-key (kbd "M-P") 'ach-move-line-up)
(global-set-key (kbd "M-N") 'ach-move-line-down)

;; Another useful one. COnvert DOuble CAps to Single Caps minor mode.
;; Source 1: http://endlessparentheses.com/fixing-double-capitals-as-you-type.html
;; Source 2: http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type/13975#13975
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

;; This minor mode is useful with Org and LaTeX. Maybe others as we
;; write

(add-hook 'text-mode-hook #'dubcaps-mode)

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'dubcaps-mode)
  (add-hook 'gfm-mode-hook #'dubcaps-mode))

(with-eval-after-load 'tex-mode
  (add-hook 'LaTeX-mode-hook #'dubcaps-mode))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'dubcaps-mode))

;; I asked a question on reddit about how to make links clickable and
;; got an answer quite quickly. Thank you reddit!
;; https://www.reddit.com/r/emacs/comments/5e94pg/have_links_in_comments_like_spacemacs/
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; More from purcell's. I once thought highlighting symbol was slowing
;; down my PC. Looks like I was wrong
(use-package highlight-symbol
  :ensure t
  :diminish ""
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  :config
  (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
                (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

;; I use this all the time
(use-package rainbow-mode
  :ensure t)

;; I often feel the need for this one
(use-package paredit-everywhere
  :ensure t
  :diminish "parev"
  :init
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
  (add-hook 'LaTeX-mode-hook #'paredit-everywhere-mode)
  (add-hook 'web-mode-hook #'paredit-everywhere-mode)
  :config
  (define-key paredit-everywhere-mode-map (kbd "C-(") 'paredit-backward-slurp-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-{") 'paredit-backward-barf-sexp)
  (define-key paredit-everywhere-mode-map (kbd "C-<backspace>") 'paredit-backward-kill-word)

  ;; paredit-kill in M-k and kill-sentence in M-K
  (define-key paredit-everywhere-mode-map (kbd "M-k") 'paredit-kill)
  (define-key paredit-everywhere-mode-map (kbd "M-K") 'kill-sentence)

  ;; interfering with bm hotkeys
  (define-key paredit-everywhere-mode-map (kbd "M-]") nil)
  )

;; This package is super useful with ivy-occur
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; xref-find-references
(global-set-key (kbd "M-?") 'xref-find-references)

;; Hungry delete mode seems very good
(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :init
  (add-hook 'prog-mode-hook 'hungry-delete-mode))

(provide 'setup-editor)
;;; setup-editor.el ends here
