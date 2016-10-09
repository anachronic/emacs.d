;;; setup-mail.el --- Mail conf file.
;;; Commentary:
;;; Code:

(when (file-exists-p "~/elisp/mail.el")
  (load-file "~/elisp/mail.el"))

;; The following settings are ~/elisp/mail.el examples.
;; (setq user-full-name        "John Doe."
;;       user-mail-address     "example@example.org"
;;       smtpmail-smtp-server  "example.org"
;;       smtpmail-smtp-service 587
;;       send-mail-function    'smtpmail-send-it)


;; We want to send mail asynchronously
(require 'smtpmail-async)
(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it)


(provide 'setup-mail)
;;; setup-mail.el ends here
