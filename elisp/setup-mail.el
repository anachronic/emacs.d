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

(setq mu4e-html2text-command
      "lynx -dump -stdin -force_html -width=72 -nolist -nobold -nocolor -display_charset UTF-8")

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/inbox"             . ?i)
        ("/archlinux"         . ?a)
        ("/academic"          . ?c)
        ("/Google Scholar"    . ?s)
        ("/promos"            . ?p)
        ("/Accounts"          . ?A)
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

;; BBDB
(use-package gmail2bbdb
  :ensure t
  :config
  (setq gmail2bbdb-bbdb-file "~/Dropbox/.bbdb"))

(use-package bbdb
  :ensure t)

(add-hook 'bbdb-initialize-hook
          '(lambda ()
             ;; @see http://emacs-fu.blogspot.com.au/2009/08/managing-e-mail-addresses-with-bbdb.html
             (setq
              bbdb-offer-save 1                        ;; 1 means save-without-asking

              bbdb-use-pop-up t                        ;; allow popups for addresses
              bbdb-electric-p t                        ;; be disposable with SPC
              bbdb-popup-target-lines  1               ;; very small

              bbdb-dwim-net-address-allow-redundancy t ;; always use full name
              bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

              bbdb-always-add-address t                ;; add new addresses to existing...
              bbdb-completion-type nil                 ;; complete on anything
              bbdb-complete-name-allow-cycling t       ;; cycle through matches
              ;; this only works partially

              bbbd-message-caching-enabled t           ;; be fast
              bbdb-use-alternate-names t               ;; use AKA

              bbdb-elided-display t                    ;; single-line addresses

              ;; auto-create addresses from mail
              bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
              bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
              ;; NOTE: there can be only one entry per header (such as To, From)
              ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

              '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter\\|notifications")))

             ;; just remove some warning since bbdb package hook the mail-mode
             (setq compose-mail-user-agent-warnings nil)))

(require 'bbdb)
(bbdb-initialize 'mu4e)
(bbdb-initialize 'message)
(bbdb-mua-auto-update-init)
(setq bbdb-file "~/Dropbox/.bbdb")
(setq bbdb-mail-user-agent (quote message-user-agent))
(setq mu4e-view-mode-hook (quote (bbdb-mua-auto-update visual-line-mode)))
(setq mu4e-compose-complete-addresses nil)
(setq bbdb-mua-pop-up t)

(add-hook 'message-mode-hook '(lambda ()
                                (progn
                                  (company-mode 1)
                                  (local-set-key (kbd "TAB") #'company-complete))))

(provide 'setup-mail)
;;; setup-mail.el ends here
