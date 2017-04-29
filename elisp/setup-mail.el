;;; setup-mail.el --- Mail conf file.
;;; Commentary:
;;; Code:

(maybe-load-file "~/Dropbox/elisp/mail.el")

;; The following settings are ~/elisp/mail.el examples.
;; (setq user-full-name        "John Doe."
;;       user-mail-address     "example@example.org"
;;       smtpmail-smtp-server  "example.org"
;;       smtpmail-smtp-service 587
;;       send-mail-function    'smtpmail-send-it)

(require 'mu4e)

;; default
(setq mu4e-maildir (expand-file-name "~/Mail"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/inbox"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap")

(setq message-citation-line-format "On %D %I:%M %p, %N wrote:"
      mail-from-style 'angles
      message-cite-style 'message-cite-style-gmail
      message-citation-line-function 'message-insert-formatted-citation-line)

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; Invoke mu4e with C-c m
(global-set-key (kbd "C-c m") 'mu4e)

(provide 'setup-mail)
;;; setup-mail.el ends here
