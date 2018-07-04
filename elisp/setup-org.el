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
  :commands (org-mode org-agenda)
  :init
  (setq-default org-agenda-files '("~/Dropbox/orgfiles/"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (ruby . t)
     (python . t)
     (R . t))))

(use-package evil-org
  :ensure t
  :after org
  :defer t
  :diminish "e"
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)
              (evil-define-key nil evil-normal-state-map
                "]t" 'org-shiftright
                "[t" 'org-shiftleft)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (require 'evil-org-agenda)
              (evil-org-agenda-set-keys))))

(let ((gcal-file "~/Dropbox/private/org-gcal-config.el"))
  (when (file-exists-p gcal-file)
    (use-package org-gcal
      :ensure t
      :defer t
      :commands (org-gcal-sync)
      :init
      (load-file gcal-file))))

(provide 'setup-org)
;;; setup-org.el ends here
