;;; setup-org.el --- Setup org mode in Emacs.
;;; Commentary:
;;; Code:


;; Agenda is cool
(global-set-key (kbd "C-c A") 'org-agenda)

(defvar nsv/org-personal "~/Dropbox/orgfiles/personal.org"
  "Personal stuff `org-mode' file.")

(defvar nsv/org-tasks "~/Dropbox/orgfiles/tasks.org"
  "University/Work related tasks.")

;; Let's load our agenda file(s)
(defvar nsv/org-agenda-files)
(setq nsv/org-agenda-files '("~/Dropbox/orgfiles/personal.org"
                             "~/Dropbox/orgfiles/tasks.org"
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

;; This needs to change some time.
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

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "CLOSED(d!)" "CANCELLED (c@/!)")))


  ;; We want to log into drawers.
  ;; https://www.youtube.com/watch?v=nUvdddKZQzs
  (setq-default org-log-into-drawer t)

  ;; Set a CLOSED timestamp for done tasks and record rescheduling
  ;; https://www.youtube.com/watch?v=R4QSTDco_w8
  (setq-default org-log-done 'time)
  (setq-default org-log-reschedule 'time)

  ;; Kill a subtree. This looks very useful
  (define-key org-mode-map (kbd "C-M-k") 'org-cut-subtree)
  )

;; Org capture
(global-set-key (kbd "C-c c") 'org-capture)

(setq-default
 org-capture-templates
 (list
  ;; Personal stuff
  `("t" "Teatro" entry
    (file+headline ,nsv/org-personal "Funciones teatrales")
    "* %?\nSCHEDULED: %^T\n:PROPERTIES:\n:Lugar: %^{Lugar|U. de Chile|Municipal|Otro}\n:END:\n")
  `("v" "Viaje" entry
    (file+headline ,nsv/org-personal "Viajes")
    "* [VIAJE] %^{Destino}\nSCHEDULED: %^T\n\n")

  ;; Work/Uni/TODO stuff
  `("m" "Responder mail" entry
    (file+headline ,nsv/org-tasks "Mail")
    "* TODO Responder (%a)"
    :immediate-finish t)
  `("w" "Tarea" entry
    (file+headline ,nsv/org-tasks "Tareas")
    "* TODO %?\nDEADLINE: %^t\n\n")
  `("r" "Reunión" entry
    (file+headline ,nsv/org-tasks "Reuniones")
    "* TODO %?\nSCHEDULED: %^T\n:PROPERTIES:\n:Lugar: %^{Lugar}\n:Personas: %^{Participantes}\n:END:\n\n"))
 )

;; Try this thing
(with-eval-after-load 'org
  (use-package org-gcal
    :ensure t
    :defer t
    :config
    (maybe-load-file "~/Dropbox/elisp/org-gcal-settings.el")
    ;; Need to refresh this thing once in a while
    (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))))

(with-eval-after-load 'org
  (use-package ox-gfm
    :ensure t
    :init
    (require 'ox-gfm nil t)))

(provide 'setup-org)
;;; setup-org.el ends here
