;;; setup-search.el --- Functions to search and jump around..
;;; Commentary:
;;; Code:

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
  :after key-chord
  :bind (("C-c j c" . iy-go-up-to-char)
         ("C-c j b" . iy-go-to-char-backward))
  :config
  (require 'key-chord)
  (key-chord-define-global "jh" #'iy-go-to-char-backward)
  (key-chord-define-global "jk" #'iy-go-up-to-char)
  (global-set-key (kbd "C-j") #'iy-go-to-or-up-to-continue)
  (global-set-key (kbd "C-S-j") #'iy-go-up-to-char-continue-backward))

;; also ace link
(use-package ace-link
  :ensure t
  :bind ("C-c j u" . ace-link))

;; I've been using occur pretty frequently.
(global-set-key (kbd "C-S-o") 'occur)

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

;; Lets use swiper conservatively. I actually like isearch better.
(use-package swiper
  :ensure t
  :bind ("C-S-s" . swiper)
  :config
  ;; I'd like to do something along the lines of isearch-swiper, which
  ;; means to switch my isearch query to swiper
  (defun isearch-swiper ()
    "Switch from isearch to swiper keeping query string."
    (interactive)
    (isearch-exit)
    (anzu--reset-mode-line)
    (swiper isearch-string))
  (define-key isearch-mode-map (kbd "C-S-s") #'isearch-swiper)
  (define-key isearch-mode-map (kbd "C-S-r") #'isearch-swiper))

(provide 'setup-search)
;;; setup-search.el ends here
