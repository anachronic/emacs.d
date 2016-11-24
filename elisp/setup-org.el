;;; setup-org.el --- Setup org mode in Emacs.
;;; Commentary:
;;; Code:


;; Use org plus contrib
(use-package org-plus-contrib
  :ensure t
  :defer t)

;; Agenda is cool
(global-set-key (kbd "C-c a") 'org-agenda)

;; org keywords. I like having more than the usual TODO/
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p!)" "|" "DONE(d!)" "CANCELLED(c@)" "WAITING(w@/!)")))

;; org-bullets bro!
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Let's load our agenda file(s)
(defvar my/org-agenda-file)
(setq my/org-agenda-file "~/Dropbox/agenda.org")

(when (file-exists-p my/org-agenda-file)
  (setq org-agenda-files (list my/org-agenda-file)))

;; reveal.js stuff Thanks to Mike Zamansky
;; https://www.youtube.com/watch?v=psDpCpcIVYs
;; I had a LOT of trouble
;; when installing for the first time. :defer t for this and
;; org-plus-contrib solved it
(use-package ox-reveal
  :ensure ox-reveal
  :defer t
  :config
  (with-eval-after-load 'org
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
    (setq org-reveal-mathjax t)))

(use-package htmlize
  :ensure t
  :defer t)

;; Ensure we can get out of org-src with C-x C-s
;; god narrow-or-widen-dwim is cool
(with-eval-after-load 'org-src
  (define-key org-src-mode-map "\C-x\C-s" #'org-edit-src-exit))

(with-eval-after-load 'org
  (setenv "PDFLATEX" "pdflatex -shell-escape")
  (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-create-formula-image-program 'imagemagick)

  (setq org-src-fontify-natively t)

  ;; Some org key bindings conflict with mine. Namely C-j. I do like to
  ;; navigate and move around quickly, but org indentation is also a
  ;; pain, which is why C-j is so cool.

  ;; Turns out since I write comments constantly when programming, I use
  ;; M-j quite frequently, and also, org mode really feels like you're
  ;; commenting stuff rather than programming, so that's a win. Let's
  ;; use that.
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "M-j") 'org-return-indent))

(provide 'setup-org)
;;; setup-org.el ends here
