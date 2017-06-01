;;; setup-org.el --- Setup org mode in Emacs.
;;; Commentary:
;;; Code:

(defvar ach-org-personal "~/Dropbox/orgfiles/personal.org"
  "Personal stuff `org-mode' file.")

(defvar ach-org-tasks "~/Dropbox/orgfiles/tasks.org"
  "University/Work related tasks.")

;; Let's load our agenda file(s)
(defvar ach-org-agenda-files)
(setq ach-org-agenda-files '("~/Dropbox/orgfiles/personal.org"
                             "~/Dropbox/orgfiles/tasks.org"
                             "~/Dropbox/orgfiles/gcal.org"))

(use-package htmlize
  :ensure t
  :defer t)

(maybe-install-packages 'org)

(ach-define-bookmark ach-org-personal "p" "personal-todo")
(ach-define-bookmark ach-org-tasks "t" "work-todo")

;; Ensure we can get out of org-src with C-x C-s
;; god narrow-or-widen-dwim is cool
(with-eval-after-load 'org-src
  (setq org-src-fontify-natively t)
  (define-key org-src-mode-map "\C-x\C-s" #'org-edit-src-exit))

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'org-indent-mode)

;; Agenda config
(global-set-key (kbd "C-c A") 'org-agenda)
(define-key meta-m-map (kbd "a") 'org-agenda)

;; Not urgent TODO keywords for agenda
(defvar ach-org-unimportant-keywords
  '("FUNCION" "VIAJE" "PROYECTO")
  "Skip these org mode keywords in alltodo agenda.")

(with-eval-after-load 'org-agenda
  (setq org-agenda-files ach-org-agenda-files)
  (setq org-agenda-custom-commands
        '(("e" "Both agenda and TODO items"
           ((alltodo ""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'todo ach-org-unimportant-keywords))
                      (org-agenda-overriding-header "Deberes generales")))
            (agenda ""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
            (todo "FUNCION"
                  ((org-agenda-overriding-header "Funciones teatrales y musicales"))))))))

(with-eval-after-load 'ox-latex
  (setenv "PDFLATEX" "pdflatex -shell-escape")
  (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-create-formula-image-program 'imagemagick))

;; This needs to change some time.
(with-eval-after-load 'org
  ;; M-j seems better than C-j for org-return-indent...
  (define-key org-mode-map (kbd "M-j") 'org-return-indent)
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
  (define-key org-mode-map (kbd "C-M-k") 'org-cut-subtree))

;; Org capture
(global-set-key (kbd "C-c c") 'org-capture)

(setq-default
 org-capture-templates
 (list
  ;; Personal stuff
  `("t" "Teatro" entry
    (file+headline ,ach-org-personal "Funciones teatrales")
    "* FUNCION %?\nSCHEDULED: %^T\n:PROPERTIES:\n:Lugar: %^{Lugar|U. de Chile|Municipal|Otro}\n:END:\n")
  `("v" "Viaje" entry
    (file+headline ,ach-org-personal "Viajes")
    "* VIAJE %^{Destino}\nSCHEDULED: %^T\n\n")
  `("p" "Proyecto" entry
    (file+headline ,ach-org-personal "Proyectos personales")
    "* PROYECTO %?\n\n")

  ;; Work/Uni/TODO stuff
  `("m" "Responder mail" entry
    (file+headline ,ach-org-tasks "Mail")
    "* TODO Responder (%a)"
    :immediate-finish t)
  `("w" "Tarea" entry
    (file+headline ,ach-org-tasks "Tareas")
    "* TAREA %?\nDEADLINE: %^t\n\n")
  `("r" "Reunión" entry
    (file+headline ,ach-org-tasks "Reuniones")
    "* REUNION %?\nSCHEDULED: %^T\n:PROPERTIES:\n:Lugar: %^{Lugar}\n:Personas: %^{Participantes}\n:END:\n\n")
  `("a" "Work assignment" entry
    (file+headline ,ach-org-tasks "Work/calce")
    "* ISSUE %?\n"))
 )

;; Exporters don't seem to go well with use-package and org
(maybe-install-packages 'ox-gfm 'ox-reveal)

(with-eval-after-load 'org
  (require 'ox-gfm nil t)
  (require 'ox-reveal nil t)
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)
  )

(provide 'setup-org)
;;; setup-org.el ends here
