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
;; Most of it comes from an excellent post by Aaron Bieber.
;; https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html
(global-set-key (kbd "C-c A") 'org-agenda)
(define-key meta-m-map (kbd "a") 'org-agenda)

(defun air-pop-to-org-agenda (&optional close)
  "Visit the org agenda, CLOSE other buffers if that value is non-nil."
  (interactive "P")
  (org-agenda nil "e")
  (when close
    (delete-other-windows)))


(global-set-key (kbd "M-SPC") 'air-pop-to-org-agenda)

;; Not urgent TODO keywords for agenda
(defvar ach-org-unimportant-keywords
  '("FUNCION" "VIAJE" "PROYECTO")
  "Skip these org mode keywords in alltodo agenda.")

(with-eval-after-load 'org-agenda
  ;; Thanks to Aaron Bieber
  ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (setq org-agenda-files ach-org-agenda-files)
  (setq org-agenda-custom-commands
        '(("e" "Both agenda and TODO items"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Para hoy:")))
            (agenda ""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (org-agenda-skip-if nil '(scheduled deadline))
                            (air-org-skip-subtree-if-priority ?A)))
                      (org-agenda-overriding-header "Todo el resto de tareas por hacer")))
            )))))

;; These are the defaults I like for exporting.
(with-eval-after-load 'ox
  (setq org-export-with-toc nil)
  (setq org-export-preserve-breaks t)
  (setq org-export-with-section-numbers t))

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
    "* ISSUE %?\n")
  `("g" "General TODO" entry
    (file+headline ,ach-org-tasks "General")
    "* TODO %?\n")
  `("G" "General TODO with link" entry
    (file+headline ,ach-org-tasks "General")
    "* TODO %A\n"
    :immediate-finish t)))

;; Capturing from the OS
;; Thank you Mike Zamansky
;; http://cestlaz.github.io/posts/using-emacs-24-capture-2/
(use-package noflet
  :ensure t)

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

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
