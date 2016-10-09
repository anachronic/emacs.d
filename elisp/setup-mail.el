;;; setup-mail.el --- Mail conf file.
;;; Commentary:
;;; Code:

(when (file-exists-p "~/elisp/mail.el")
  (load-file "~/elisp/mail.el"))
;; (setq send-mail-function    'smtpmail-send-it
;;       smtpmail-smtp-server  "example.org"
;;       smtpmail-stream-type  'ssl
;;       smtpmail-smtp-service 587)

(provide 'setup-mail)
;;; setup-mail.el ends here
