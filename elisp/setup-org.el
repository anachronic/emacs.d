
(global-set-key (kbd "C-c a") 'org-agenda)

;; Let's load our agenda file(s)
(setq my/org-agenda-file "~/Dropbox/agenda.org")

(when (file-exists-p my/org-agenda-file)
  (setq org-agenda-files (list my/org-agenda-file)))



(provide 'setup-org)
;;; setup-org.el ends here
