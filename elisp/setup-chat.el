;;; setup-chat.el --- ERC configuration.
;;; Commentary:
;;; Code:


;; own user related stuff: nick, whatever.
(maybe-load-file "~/Dropbox/elisp/chat.el")

(with-eval-after-load 'erc
  (setq erc-prompt (lambda () (concat (buffer-name) " >>>"))))


(provide 'setup-chat)
;;; setup-chat.el ends here
