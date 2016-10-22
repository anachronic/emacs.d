;;; setup-editor.el --- Set up everything associated with buffer manipulation.
;;; Commentary:
;;; Code:

(require 'hl-line)
(show-paren-mode 1)
(global-hl-line-mode)

;; Make C-n add newlines at end of file
(setq next-line-add-newlines t)

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

;; I'm a little bit too young for emacs,
;; so I like replacing the region with stuff.
(delete-selection-mode 1)


;; Wrapping the buffer is very useful in org mode and latex mode
(defun my/visualmode ()
  "Setup visual line mode in the buffer."
  (visual-line-mode))

(add-hook 'org-mode-hook 'my/visualmode)
(add-hook 'Man-mode-hook 'my/visualmode)

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

;; I want C-x k to delete the current buffer, not to ask. I can do that with C-x C-k...
(global-set-key (kbd "C-x k") 'kill-this-buffer)


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
    (add-hook 'after-init-hook 'global-color-identifiers-mode)
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

(provide 'setup-editor)
;;; setup-editor.el ends here
