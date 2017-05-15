;;; setup-editor.el --- Set up everything associated with buffer manipulation.
;;; Commentary:
;;; Code:

;; Well, this mode seemed like a good idea. But it's not really that
;; useful. Editing feels really sluggish and I really dislike that. So
;; let's use just for absolute and NON-INTRUSIVE keys (like semicolor)
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

;; Visual fill column for text-only buffers, like mail and feeds. and maybe org..
(use-package visual-fill-column
  :ensure t)

;; hydra
(use-package hydra
  :ensure t)

(require 'hl-line)
(show-paren-mode 1)
(global-hl-line-mode)

;; Enable narrow commands
(put 'narrow-to-region 'disabled nil)

;; Flycheck. What's an editor without error checking?
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit))

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

;; expand region. An *excellent* tool.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Undo-Tree for real undo/redo commands
(use-package undo-tree
  :ensure t
  :diminish ""
  :config
  (global-undo-tree-mode 1)
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
  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-toggle-all-nodes))
  :config
  (add-hook 'prog-mode-hook #'origami-mode))

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
(defun nsv/indent-whole-buffer ()
  "Indent the whole buffer according to the defined style."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "C-c TAB") 'nsv/indent-whole-buffer)

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
(defun nsv/open-line-above ()
  "Insert a newline before the current line and leave point on it."
  (interactive)
  (push-mark)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "C-o") 'nsv/open-line-above)

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
(defun nsv/duplicate-the-line ()
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

(global-set-key (kbd "C-c d") 'nsv/duplicate-the-line)

;; My version of transpose lines. While emacs' transpose lines does the job,
;; I like the IntelliJ/Pycharm implementation better. It's cleaner. So let's
;; do that

(defun nsv/move-line-up ()
  "Transpose the current line with the one above leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-beginning-of-line 1)
    (forward-char col)))


(defun nsv/move-line-down ()
  "Transpose the current line with the one below leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-beginning-of-line 1)
    (forward-char col)))

(global-set-key (kbd "M-P") 'nsv/move-line-up)
(global-set-key (kbd "M-N") 'nsv/move-line-down)

;; Kill to beginning of line
;; not sure about binding this one yet.
(defun nsv/kill-to-line-beg ()
  "Kill to beginning of line."
  (interactive)
  (set-mark-command nil)
  (back-to-indentation)
  (kill-region (region-beginning) (region-end)))


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

;; Navigating hydra. This turned out to be very useful with key-chord
;; when you want to navigate a text. Cool stuff. I actually like 'kk'
;; better for it rather than jj
(defhydra hydra-text (:columns 3)
  "Movement and text manipulation hydra"
  ("i" (forward-line -1) "up")
  ("k" (forward-line 1) "down")
  ("j" backward-char "back")
  ("l" forward-char "forward")
  ("dd" kill-whole-line "kill the whole line")
  ("de" kill-line "kill until the end of the line")
  ("da" nsv/kill-to-line-beg "kill until beginning of line")
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
(add-hook 'LaTeX-mode-hook #'dubcaps-mode)
(add-hook 'org-mode-hook #'dubcaps-mode)
(add-hook 'markdown-mode-hook #'dubcaps-mode)
(add-hook 'gfm-mode-hook #'dubcaps-mode)
(add-hook 'text-mode-hook #'dubcaps-mode)

;; I've ran into this situation where I really need to insert some paragraphs or
;; stuff, so let's use lorem-ipsum
(use-package lorem-ipsum
  :ensure t
  :config
  (lorem-ipsum-use-default-bindings))

;; Let's use hippie expand.
(global-set-key (kbd "M-/") #'hippie-expand)

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

;; I saw this in purcell's config, looks useful
(use-package indent-guide
  :ensure t
  :diminish ""
  )

;; I use this all the time
(use-package rainbow-mode
  :ensure t)

;; I often feel the need for this one
(use-package paredit-everywhere
  :ensure t
  :diminish "parev"
  :config
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
  (add-hook 'LaTeX-mode-hook #'paredit-everywhere-mode))

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

(provide 'setup-editor)
;;; setup-editor.el ends here
