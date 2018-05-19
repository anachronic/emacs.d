;;; setup-gnus.el --- Nothing substantial about this file..
;;; Commentary:
;;; Code:

(require 'f)
(setq-default gnus-init-file (f-join user-emacs-directory "gnus.el"))

;; Gnus display
(setq-default
 gnus-summary-line-format "%U%R%d %[ %-23,23f %] %B%s\n")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Self info and smtp
(setq-default message-send-mail-function 'message-send-mail-with-sendmail
              sendmail-program "/usr/bin/msmtp"
              mail-specify-envelope-from t
              message-sendmail-envelope-from 'header
              mail-envelope-from 'header
              message-default-mail-headers "Cc: \n")

(setq user-full-name "Nicolás Salas V.")
(setq user-mail-address "nikosalas@gmail.com")

(provide 'setup-gnus)
;;; setup-gnus.el ends here
