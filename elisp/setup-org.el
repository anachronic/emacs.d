;;; setup-org.el --- Setup org mode in Emacs.
;;; Commentary:
;;; Code:


;; Agenda is cool
(global-set-key (kbd "C-c A") 'org-agenda)

;; org-bullets bro!
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Let's load our agenda file(s)
(defvar nsv/org-agenda-files)
(setq nsv/org-agenda-files '("~/Dropbox/agenda.org"
                            "~/Dropbox/orgfiles/gcal.org"))

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

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'org-indent-mode)

(with-eval-after-load 'org
  (setenv "PDFLATEX" "pdflatex -shell-escape")
  (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-create-formula-image-program 'imagemagick)

  (setq org-src-fontify-natively t)

  ;; M-j seems better than C-j for org-return-indent...
  (define-key org-mode-map (kbd "M-j") 'org-return-indent)

  (setq org-agenda-files nsv/org-agenda-files)
  ;; org keywords. I like having more than the usual TODO/
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p!)" "|" "DONE(d!)" "CANCELLED(c@)" "WAITING(w@/!)"))))

;; Try this thing
(with-eval-after-load 'org
  (use-package org-gcal
    :ensure t
    :defer t
    :config
    (maybe-load-file "~/Dropbox/elisp/org-gcal-settings.el")))

;; Need to refresh this thing once in a while
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))

(provide 'setup-org)
;;; setup-org.el ends here
