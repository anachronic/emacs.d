;;; setup-org.el --- Setup org mode in Emacs.
;;; Commentary:
;;; Code:

;; Agenda is cool
(global-set-key (kbd "C-c a") 'org-agenda)

;; org keywords. I like having more than the usual TODO/
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; org-bullets bro!
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Let's load our agenda file(s)
(setq my/org-agenda-file "~/Dropbox/agenda.org")

(when (file-exists-p my/org-agenda-file)
  (setq org-agenda-files (list my/org-agenda-file)))

;; reveal.js stuff
;; Thanks to Mike Zamansky
;; https://www.youtube.com/watch?v=psDpCpcIVYs
(use-package ox-reveal
  :ensure ox-reveal
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t))

(use-package htmlize
  :ensure t)

;; Ensure we can get out of org-src with C-x C-x
;; god narrow-or-widen-dwim is cool
(eval-after-load 'org-src
  '(define-key org-src-mode-map "\C-x\C-s" #'org-edit-src-exit))

(setenv "PDFLATEX" "pdflatex -shell-escape")
(setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-create-formula-image-program 'imagemagick)

(setq org-src-fontify-natively t)

(provide 'setup-org)
;;; setup-org.el ends here
