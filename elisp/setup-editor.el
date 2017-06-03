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

;; Highlight line.
(require 'hl-line)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; Enable narrow commands
(put 'narrow-to-region 'disabled nil)

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
defun, whichever applies first. Narrowing to
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

;; Linum relative for good editing
(use-package nlinum-relative
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-redisplay-delay 0))

;; I'm a little bit too young for emacs,
;; so I like replacing the region with stuff.
(delete-selection-mode 1)

;; Recenter positions
(setq-default recenter-positions '(top middle bottom))

(defvar ach-recenter-offset 6
  "Number of lines to offset from limit when recentering.")

;; Advice `recenter-top-bottom' for offset
(defun ach-recenter-advice (orig-fun &rest args)
  "Advice to offset `recenter-top-bottom' (ORIG-FUN) called with ARGS."
  (apply orig-fun args)
  (let ((top (or (eq recenter-last-op 'top)
                 (eq recenter-last-op (car recenter-positions))))
        (bottom (eq recenter-last-op 'bottom)))
    (when ach-recenter-offset
      (cond
       (top (scroll-down ach-recenter-offset))
       (bottom (scroll-up ach-recenter-offset))))))

(advice-add 'recenter-top-bottom :around #'ach-recenter-advice)

;; expand region. An *excellent* tool.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Undo-Tree for real undo/redo commands
(use-package undo-tree
  :ensure t
  :diminish ""
  :demand
  :config
  (global-undo-tree-mode 1)
  (define-key undo-tree-map (kbd "C-/") nil)
  :bind
  (("C-z" . undo)
   ("C-S-z" . undo-tree-redo)))


(add-hook 'Man-mode-hook 'visual-line-mode)

;; diminish visual-line-mode
(diminish 'visual-line-mode)

;; might as well explicitly tell emacs we don't like tabs
(setq-default indent-tabs-mode nil)

;; smart comment was crap. Let's stick to the good ol' evil-nerd-commenter
(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Folding seems way better with origami
(use-package origami
  :ensure t
  :bind (("C-c F" . origami-toggle-all-nodes)
         ("C-c t" . origami-toggle-node))
  :defer t
  :init
  (add-hook 'prog-mode-hook #'origami-mode)

  ;; Expand origami folded nodes with TAB!
  (defun origami-closed-here-p ()
    (save-excursion
      (move-end-of-line 1)
      (backward-char 1)
      (let* ((path (origami-search-forward-for-path (current-buffer) (point)))
             (node (-last-item path)))
        (origami-fold-node-recursively-closed? node))))

  (defun origami-expand-or-TAB (&optional ARG)
    (interactive)
    (if (origami-closed-here-p)
        (save-excursion
          (move-end-of-line 1)
          (origami-show-node (current-buffer) (1- (point))))
      (call-interactively 'indent-for-tab-command)))

  (global-set-key [remap indent-for-tab-command] 'origami-expand-or-TAB))

;; I've always liked coloring the buffer, because it makes easier to identify stuff around
;; So let's test this mode
(use-package color-identifiers-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'color-identifiers-mode)
  :diminish color-identifiers-mode)

;; Deleting stuff backwards
(global-set-key (kbd "M-D") 'backward-kill-word)

;; Let's use multiple cursors.
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

;; =======Inserting lines, duplicating, etcetera.=======
;; This one comes directly from ha's config:
;; https://github.com/howardabrams/dot-files/blob/master/emacs-fixes.org
(defun newline-for-code ()
  "Insert a newline as if RET was pressed on the end of this line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "M-RET") 'newline-for-code)

;; C-o's default behaviour is kind of poor, so lets simulate vim's o.
(defun ach-open-line-above ()
  "Insert a newline before the current line and leave point on it."
  (interactive)
  (push-mark)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (unless (eq major-mode 'message-mode)
    (indent-for-tab-command)))

(global-set-key (kbd "C-o") 'ach-open-line-above)
(global-set-key [(shift return)] 'ach-open-line-above)

;; I have been using M-m lately, and I have to say I'm able to remember
;; stuff rather easily. But it is always better when stuff gets simpler.
;; Once again, browsing ha's config, I came across this function.
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

;; This should be a macro, but let's define it as a function
;; I want it to behave exactly like PyCharm or IntelliJ Idea's C-d
(defun ach-duplicate-the-line ()
  "Duplicate the current line below and set the point in the same column in the new line."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (set-mark-command nil)
    (move-end-of-line 1)
    (kill-ring-save (region-beginning) (region-end))
    (newline)
    (yank))
  (forward-line 1))

(global-set-key (kbd "C-c d") 'ach-duplicate-the-line)

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

;; ==================== END of line manipulation functions =====

;; occur next-prev. Actually it can be used with errors too
(global-set-key (kbd "M-s M-p") 'previous-error)

;; I've ran into the situation where I want to zap stuff (specially
;; with paredit). And I was trying zzz-up-to-char. But the extra key
;; press isn't quite the right solution for the job: you see, if the
;; region you want to kill is large, you will -as the word suggests-
;; use a *region*. My mindset is that i'll only use zap to and up to
;; char when the text region is short, which is why I do *not* want an
;; extra key press, I just want the job done. I found the solution at
;; purcell's emacs, magnars' and the irreal blog. Zapping can be very
;; useful
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")

;; M-z for up to and M-Z for the including variant.
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; For zapping backwards needs negative prefix. Need a lot of muscle
;; memory for that.

(defhydra hydra-text (:columns 3)
  "Movement and text manipulation hydra"
  ("i" (forward-line -1) "up")
  ("k" (forward-line 1) "down")
  ("j" backward-char "back")
  ("l" forward-char "forward")
  ("dd" kill-whole-line "kill the whole line")
  ("de" kill-line "kill until the end of the line")
  ("da" ach-kill-to-line-beg "kill until beginning of line")
  ("a" beginning-of-line "beginning of line")
  ("e" end-of-line "end of line")
  ("x" delete-char "kill char at point")
  ("z" zap-up-to-char "zap up to char")
  ("u" undo-tree-undo "undo")
  ("r" undo-tree-redo "redo")
  ("wc" capitalize-word "capitalize word")
  ("wd" downcase-word "downcase word")
  ("wu" upcase-word "uppercase word")
  ("h" nil "quit (insert mode)" :color blue)
  ("q" nil "quit" :color blue))

;; I found out this code snippet that while it doesn't really work in
;; the scratch buffer, it does look useful. We shall see. Should be
;; useful when reading long stuff URL:
;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)


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

;; I've ran into this situation where I really need to insert some paragraphs or
;; stuff, so let's use lorem-ipsum
(use-package lorem-ipsum
  :ensure t
  :config
  (lorem-ipsum-use-default-bindings))

;; I asked a question on reddit about how to make links clickable and
;; got an answer quite quickly. Thank you reddit!
;; https://www.reddit.com/r/emacs/comments/5e94pg/have_links_in_comments_like_spacemacs/
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; More from purcell's. I once thought highlighting symbol was slowing
;; down my PC. Looks like I was wrong
(use-package highlight-symbol
  :ensure t
  :diminish ""
  :config
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
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
  )

;; Been using M-w quite a while and yes, @purcell is right, it is
;; often the line I have to copy
(use-package whole-line-or-region
  :ensure t
  :diminish ""
  :config
  (add-hook 'after-init-hook 'whole-line-or-region-mode))

;; This package is super useful with ivy-occur
(use-package wgrep
  :ensure t)

;; Edit current file as sudo
;; From spacemacs, which took this from magnars:
(defun ach-sudo-edit (&optional arg)
  "Edit current file as sudo, prompt for file instead if ARG is present."
  (interactive "P")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(global-set-key (kbd "C-c E") 'sudo-edit)


(provide 'setup-editor)
;;; setup-editor.el ends here
