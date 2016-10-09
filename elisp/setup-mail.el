;;; setup-mail.el --- Mail conf file.
;;; Commentary:
;;; Code:

(when (file-exists-p "~/elisp/mail.el")
  (load-file "~/elisp/mail.el"))


;; (setq user-full-name        "John Doe."
;;       user-mail-address     "example@example.org"
;;       smtpmail-smtp-server  "example.org"
;;       smtpmail-smtp-service 587
;;       send-mail-function    'smtpmail-send-it)


(provide 'setup-mail)
;;; setup-mail.el ends here
