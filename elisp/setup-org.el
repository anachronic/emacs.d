;;; setup-org.el --- Setup org mode in Emacs.
;;; Commentary:
;;; Code:

;; Honestly this section deserves attention once I USE org mode. I've
;; been doing it so i get packages and configure them but never
;; actually use them. This is no way to start any kind of project nor
;; is it a way to get packages. I'll get stuff once I need the stuff
(use-package org
  :ensure t
  :defer t
  :commands (org-mode org-agenda org)
  :config
  (setq org-agenda-files '("~/Dropbox/orgfiles/")))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     ;; From purcell
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (ruby . t)
     (python . t)
     (R . t))))

(use-package evil-org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)
              (evil-define-key nil evil-normal-state-map
                "]t" 'org-shiftright
                "[t" 'org-shiftleft)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (require 'evil-org-agenda)
              (evil-org-agenda-set-keys)))
  )

(use-package org-gcal
  :ensure t
  :defer t
  :init
  (maybe-load-file "~/Dropbox/private/org-gcal-config.el"))

(provide 'setup-org)
;;; setup-org.el ends here
