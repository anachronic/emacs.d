;;; setup-chat.el --- ERC configuration.
;;; Commentary:
;;; Code:


;; own user related stuff: nick, whatever.
(when (file-exists-p "~/Dropbox/elisp/chat.el")
  (load-file "~/Dropbox/elisp/chat.el"))

(require 'erc)
(setq erc-prompt (lambda () (concat (buffer-name) " >>>")))


(provide 'setup-chat)
;;; setup-chat.el ends here
