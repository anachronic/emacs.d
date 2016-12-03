;;; setup-mail.el --- Mail conf file.
;;; Commentary:
;;; Code:

(let ((mailfile "~/elisp/mail.el"))
  (when (file-exists-p mailfile)
    (load-file mailfile)))

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

;; And finally, Notmuch is really the way to go with mail
(use-package notmuch
  :ensure t
  :config
  (global-set-key (kbd "C-c m") #'notmuch)
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-text/html-blocked-images nil))

(with-eval-after-load 'time
  (setq display-time-use-mail-icon t)
  (setq display-time-24hr-format t)
  (setq display-time-load-average-threshold 100)
  (setq display-time-mail-function
        (lambda ()
          (not (eq 0
                   (string-to-number
                    (shell-command-to-string
                     "notmuch search tag:inbox,unread | wc -l")))))))


(provide 'setup-mail)
;;; setup-mail.el ends here
