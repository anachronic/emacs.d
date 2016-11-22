;;; setup-editor.el --- Set up everything associated with buffer manipulation.
;;; Commentary:
;;; Code:

(require 'hl-line)
(show-paren-mode 1)
;; (global-hl-line-mode)

;; Make C-n add newlines at end of file
(setq next-line-add-newlines t)

;; Enable narrow commands
(put 'narrow-to-region 'disabled nil)

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

;; Let's bind it to C-c n and C-x n
(global-set-key (kbd "C-c n") #'narrow-or-widen-dwim)
(define-key ctl-x-map (kbd "n") #'narrow-or-widen-dwim)

;; The last instruction isn't as good without this next instruction
;; Yes, you guessed it, it deletes all trailing whitespaces and newlines..
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Linum relative for good editing
(use-package nlinum-relative
  :ensure t
  :config
  ;; something else you want
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-redisplay-delay 0.2))


;; I'm a little bit too young for emacs,
;; so I like replacing the region with stuff.
(delete-selection-mode 1)


;; Wrapping the buffer is very useful in org mode and latex mode
(defun my/visualmode ()
  "Setup visual line mode in the buffer."
  (visual-line-mode))

(add-hook 'org-mode-hook 'my/visualmode)
(add-hook 'Man-mode-hook 'my/visualmode)
(add-hook 'gfm-mode-hook 'my/visualmode)
(add-hook 'markdown-mode-hook 'my/visualmode)

;; Also, autosave in LaTeX mode
(add-hook 'LaTeX-mode-hook 'my/visualmode)
(setq TeX-auto-save t)


;; diminish visual-line-mode
(diminish 'visual-line-mode)

;; might as well explicitly tell emacs we don't like tabs
(setq-default indent-tabs-mode nil)

;; I have had some problems with the tab key and AC/company
;; This could fix it. Not sure.
(setq-default tab-always-indent 'complete)

;; So i was using kill-this-buffer for C-x k. But sometimes I don't really
;; want to kill the buffer, I just want it to get it out of the way.
;; So let's bury the buffer with the same key when a prefix is specified.
(defun my/kill-buffer-or-bury-dwim (&optional arg)
  "If ARG, bury buffer, otherwise kill the buffer."
  (interactive "P")
  (if arg
      (bury-buffer)
    (kill-this-buffer)))

(define-key ctl-x-map (kbd "k") 'my/kill-buffer-or-bury-dwim)

;; According to http://oremacs.com/2015/02/18/undo-nonsense/, find-file-read-only
;; is a trashy command. Whatever. Who cares about useless commands, we don't even use
;; the useful ones sometimes, right?
;; That's right. But hold on. This trashy command is bound to C-x C-r. Let's get
;; rid of it and bind it to a useful command: revert buffer
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer nil t)))

;; While I once thought avy was cool, it isn't really what I thought
;; it would be. I'd much rather use isearch and iy-go-up-to-char
(use-package avy
  :ensure t
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)))

;; So let's use iy-go-up-to-char. I hardly ever use (and this is to be
;; precautious, because i actually NEVER use it) C-j. It's a really
;; valuable key that's lost. mainly because I always use Emacs with
;; Gtk. I tend to use vim while in a terminal. It's also useful for
;; navegation. I guess I could set it to the "normal" C-j if last
;; command wasn't an iy-related command. But that's just too much work
;; and I'll most likely never use that.
;; It's a shame that it is related to some weird key binding. I had to
;; look it up. But it all works in the end.
;; solution: http://stackoverflow.com/a/2253044
(use-package iy-go-to-char
  :ensure t
  :bind (("C-c j c" . iy-go-up-to-char)
         ("C-c j b" . iy-go-to-char-backward))
  :config
  (global-set-key (kbd "C-j") #'iy-go-to-or-up-to-continue)
  (global-set-key (kbd "C-S-j") #'iy-go-up-to-char-continue-backward))

;; also ace link
(use-package ace-link
  :ensure t
  :bind ("C-c j u" . ace-link))


;; smart comment was crap. Let's stick to the good ol' evil-nerd-commenter
(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Let's use HideShow to fold code in long files, shall we?
;; this idea i got from Howard Abrams' dotfiles.
(defun ha/hs-show-all ()
  "Show the whole buffer.  In other words: don't fold any code."
  (interactive)
  (hs-minor-mode 1)
  (hs-show-all))

(defun ha/hs-hide-all ()
  "Fold everything in the current buffer."
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-all))

(defun ha/hs-toggle-hiding ()
  "Fold/unfold the current fundef."
  (interactive)
  (hs-minor-mode 1)
  (hs-toggle-hiding))

(global-set-key (kbd "C-c f h") 'ha/hs-hide-all)
(global-set-key (kbd "C-c f t") 'ha/hs-toggle-hiding)
(global-set-key (kbd "C-c f s") 'ha/hs-show-all)

;; and let's get rid of the minor mode since these functions toggle it anyway
(diminish 'hs-minor-mode)


;; I've always liked coloring the buffer, because it makes easier to identify stuff around
;; So let's test this mode
(use-package color-identifiers-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'global-color-identifiers-mode)
  :diminish color-identifiers-mode)


;; Deleting stuff
(global-set-key (kbd "C-S-d") 'delete-backward-char)
(global-set-key (kbd "M-D") 'backward-kill-word)

;; I like prettify symbols mode. but only for elisp
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

;; Let's use multiple cursors.
(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

;; According to: https://github.com/joaotavora/autopair,
;; electric-pair-mode is *better* than autopair
(electric-pair-mode)

;; Indent the whole buffer
(defun my/indent-whole-buffer ()
  "Indent the whole buffer according to the defined style."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "C-c TAB") 'my/indent-whole-buffer)

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
(defun my/open-line-above ()
  "Insert a newline before the current line and leave point on it."
  (interactive)
  (push-mark)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))

(global-set-key (kbd "C-o") 'my/open-line-above)

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
(defun my/duplicate-the-line ()
  "Duplicate the current line below and set the point in the same column in the new line."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (set-mark-command nil)
    (move-end-of-line 1)
    (kill-ring-save (region-beginning) (region-end))
    (newline)
    (yank))
  (next-line))

(global-set-key (kbd "C-c d") 'my/duplicate-the-line)

;; My version of transpose lines. While emacs' transpose lines does the job,
;; I like the IntelliJ/Pycharm implementation better. It's cleaner. So let's
;; do that

(defun my/move-line-up ()
  "Transpose the current line with the one above leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (previous-line 2)
    (move-beginning-of-line 1)
    (forward-char col)))


(defun my/move-line-down ()
  "Transpose the current line with the one below leaving the cursor in the first line."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (previous-line 1)
    (move-beginning-of-line 1)
    (forward-char col)))

(global-set-key (kbd "M-P") 'my/move-line-up)
(global-set-key (kbd "M-<up>") 'my/move-line-up)

(global-set-key (kbd "M-<down>") 'my/move-line-down)
(global-set-key (kbd "M-N") 'my/move-line-down)


;; Kill to beginning of line
;; not sure about binding this one yet.
(defun my/kill-to-line-beg ()
  "Kill to beginning of line."
  (interactive)
  (set-mark-command nil)
  (back-to-indentation)
  (kill-region (region-beginning) (region-end)))


;; ==================== END of line manipulation functions =====

;; occur next-prev. Actually it can be used with errors too
(global-set-key (kbd "M-s M-p") 'previous-error)

;; I've been using occur pretty frequently.
(global-set-key (kbd "C-S-o") 'occur)


;; navigating hydra
(defhydra hydra-text (:columns 3)
  "Movement and text manipulation hydra"
  ("i" previous-line "up")
  ("k" next-line "down")
  ("j" backward-char "back")
  ("l" forward-char "forward")
  ("dd" kill-whole-line "kill the whole line")
  ("de" kill-line "kill until the end of the line")
  ("da" my/kill-to-line-beg "kill until beginning of line")
  ("a" beginning-of-line "beginning of line")
  ("e" end-of-line "end of line")
  ("x" delete-char "kill char at point")
  ("z" zzz-up-to-char "zzz up to char")
  ("u" undo-tree-undo "undo")
  ("r" undo-tree-redo "redo")
  ("wc" capitalize-word "capitalize word")
  ("wd" downcase-word "downcase word")
  ("wu" upcase-word "uppercase word")
  ("h" nil "quit (insert mode)" :color blue)
  ("q" nil "quit" :color blue))

;; Transpose words is cool. just not that important
(global-set-key (kbd "M-t") 'hydra-text/body)

;; I've ran into the situation where I want to zap stuff (specially
;; with paredit). And I was trying zzz-up-to-char. But the extra key
;; press isn't quite the right solution for the job: you see, if the
;; region you want to kill is large, you will -as the word suggests-
;; use a *region*. My mindset is that i'll only use zap to and up to
;; char when the text region is short, which is why I do *not* want an
;; extra key press, I just want the job done. I found the solution at
;; purcell's emacs, magnars' and the irreal blog. Zapping can be very
;; useful
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;; M-z for up to and M-Z for the including variant.
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; For zapping backwards needs negative prefix. Need a lot of muscle
;; memory for that.


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

;; I'll be trying Paredit. Should be quite useful for lisp-like stuff
;; Yeah, I've been becoming more fond of paredit every day. It rocks!
;; However, I don't use C-j for anything, since electric indent does
;; everything for me, and if I need a key binding for newline, well, I
;; already have C-m.
(use-package paredit
  :ensure t
  :diminish "par"
  :config
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "M-;") nil)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (global-set-key (kbd "M-K") #'paredit-kill))

;; I've been burying buffers like crazy lately because it feels more
;; natural than killing them. However, I'd like to have the
;; possibility to bury the current buffer with C-q and bury the other
;; buffer with a prefix argument. This will override quoted insert,
;; but that's ok since i hardly ever use it. Let's just rebind that to
;; C-c q
(defun my/bury-buffer-dwim (arg)
  "Bury current buffer, if ARG is not nil, bury other-window's buffer instead."
  (interactive "P")
  (if arg
      (progn
        (other-window 1)
        (bury-buffer)
        (other-window -1))
    (bury-buffer)))

(global-set-key (kbd "C-q") 'my/bury-buffer-dwim)
(global-set-key (kbd "C-c q") 'quoted-insert)

;; I've ran into this situation where I really need to insert some paragraphs or
;; stuff, so let's use lorem-ipsum
(use-package lorem-ipsum
  :ensure t
  :config
  (lorem-ipsum-use-default-bindings))

;; Let's use hippie expand.
(global-set-key (kbd "M-/") #'hippie-expand)

;; I gave it a try. looks pretty cool.
(use-package anzu
  :ensure t
  :diminish ""
  :config
  ;; Spaceline already has the config. So let's remove anzu's modeline toggle.
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp))


;; I asked a question on reddit about how to make links clickable and
;; got an answer quite quickly. Thank you reddit!
;; https://www.reddit.com/r/emacs/comments/5e94pg/have_links_in_comments_like_spacemacs/
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; Also make this available in Markdown
(add-hook 'markdown-mode-hook 'goto-address-mode)

;; Stealing conf from purcell's .emacs.d. This package is actually
;; pretty cool, it gets you out of the dullness of full white text (or
;; should I say: default face)
(use-package highlight-quoted
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

;; More from purcell's. I once thought highlighting symbol was slowing
;; down my PC. Looks like I was wrong
(use-package highlight-symbol
  :ensure t
  :diminish ""
  :config
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
                (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

;; We continue the stealing: hl-sexp. Useful sometimes
(use-package hl-sexp
  :ensure t)

;; More stealing: immortal scratch. I wish I knew about this one
;; before
(use-package immortal-scratch
  :ensure t
  :config
  (add-hook 'after-init-hook #'immortal-scratch-mode))

(provide 'setup-editor)
;;; setup-editor.el ends here
