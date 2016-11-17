;;; setup-editor.el --- Set up everything associated with buffer manipulation.
;;; Commentary:
;;; Code:

(require 'hl-line)
(show-paren-mode 1)
(global-hl-line-mode)

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

;; I want to be able to comment stuff easily.

;; Change window with F12 should be good
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-<f12>") (lambda () (interactive) (other-window -1)))

;; Some bindings from https://www.masteringemacs.org/article/my-emacs-keybindings
(global-set-key (kbd "M-o") 'other-window)


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

;; I DO NOT LIKE TYPING YES!!!!
(fset 'yes-or-no-p 'y-or-n-p)

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

;; avy is a cool package that lets you navigate easily
;; I mainly use this to get rid of C-[npbf] nonsense
(use-package avy
  :ensure t
  :bind (("C-c j c" . avy-goto-char)
         ("C-c j l" . avy-goto-line)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j j" . avy-goto-word-0)
         ("M-g g" . avy-goto-line)))

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
  :init
    (add-hook 'prog-init-hook 'global-color-identifiers-mode)
  :diminish color-identifiers-mode)


;; Maximize and minimize windows
(global-set-key (kbd "C-c C-m") 'maximize-window)
(global-set-key (kbd "C-c C-S-m") 'minimize-window)


;; I want to be able to scroll without moving the point
;; source: https://www.emacswiki.org/emacs/Scrolling
(defun my/scrolldown (n)
  "Scroll down N lines without moving the point."
  (interactive "p")
  (dotimes (i n)
    (scroll-up-command 1)))

(defun my/scrollup (n)
  "Scroll up N lines without moving the point."
  (interactive "p")
  (dotimes (i n)
    (scroll-down-command 1)))

(global-set-key (kbd "M-n") 'my/scrolldown)
(global-set-key (kbd "M-p") 'my/scrollup)

;; Maybe this should be C-M-d
(global-set-key (kbd "C-S-d") 'delete-backward-char)

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

;; This looks handy. I'll have to test it though because I'm not very
;; used to it
(use-package zzz-to-char
  :ensure t
  :bind (("M-z"   . zzz-up-to-char)
         ("C-M-z" . zzz-to-char)))

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
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; At our own risk we'll use this binding. But burying a buffer is kind of essential
(global-set-key (kbd "C-c C-k") 'bury-buffer)
(global-set-key (kbd "C-c q") 'bury-buffer)

;; I've ran into this situation where I really need to insert some paragraphs or
;; stuff, so let's use lorem-ipsum
(use-package lorem-ipsum
  :ensure t
  :config
  (lorem-ipsum-use-default-bindings))

;; Let's use hippie expand.
(global-set-key (kbd "C-/") #'hippie-expand)

(provide 'setup-editor)
;;; setup-editor.el ends here
