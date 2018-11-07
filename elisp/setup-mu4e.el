;;; setup-mu4e.el --- Nothing substantial about this file..
;;; Commentary:
;;; Code:

;; (use-package mu4e
;;   :ensure nil ;; System provides this package
;;   :commands (mu4e)
;;   :config
;;   (setq
;;    mu4e-maildir       "~/.mail"           ;; top-level Maildir
;;    mu4e-sent-folder   "/icloud/sent"       ;; folder for sent messages
;;    mu4e-drafts-folder "/icloud/drafts"     ;; unfinished messages
;;    mu4e-trash-folder  "/icloud/trash"      ;; trashed messages
;;    mu4e-refile-folder "/icloud/archive")   ;; saved messages

;;   ;; custom config
;;   (setq mu4e-attachment-dir "~/Downloads"
;;         mu4e-compose-signature-auto-include nil
;;         mu4e-maildir-shortcuts '(("/icloud/Inbox" . ?i)
;;                                  ("/icloud/archive" . ?a)
;;                                  ("/icloud/trash" . ?t)
;;                                  ("/icloud/drafts" . ?d)
;;                                  ("/icloud/sent" . ?s))
;;         mu4e-completing-read-function 'ivy-completing-read
;;         mu4e-sent-messages-behavior 'delete
;;         mu4e-change-filenames-when-moving t
;;         mu4e-view-show-addresses t
;;         mu4e-view-show-images t
;;         ;; mu4e-use-fancy-chars t
;;         mu4e-confirm-quit nil
;;         mail-user-agent 'mu4e-user-agent
;;         mu4e-html2text-command "elinks -dump"
;;         mu4e-update-interval 300
;;         mu4e-get-mail-command "mbsync icloud"
;;         )

;;   ;; view in browser
;;   (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))

;;   ;; Some useful hooks for this
;;   (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
;;   (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;;   ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
;;   ;; That fixes the trash bug in mu4e
;;   (defun ach--remove-nth-element (nth list)
;;     (if (zerop nth) (cdr list)
;;       (let ((last (nthcdr (1- nth) list)))
;;         (setcdr last (cddr last))
;;         list)))
;;   (setq mu4e-marks (ach--remove-nth-element 5 mu4e-marks))
;;   (add-to-list 'mu4e-marks
;;                '(trash
;;                  :char ("d" . "▼")
;;                  :prompt "dtrash"
;;                  :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
;;                  :action (lambda (docid msg target)
;;                            (mu4e~proc-move docid
;;                                            (mu4e~mark-check-target target) "-N"))))
;;   )

(provide 'setup-mu4e)
;;; setup-mu4e.el ends here
