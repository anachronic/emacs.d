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

(require 'message)

(setq mail-from-style 'angles
      message-cite-style 'message-cite-style-gmail)

;; We'll be using gnus
(setq-default gnus-init-file
              (concat user-emacs-directory
                      "gnus/.gnus"))

;; Sometimes, thunderbird reply is better. I still get a lot of gmail
;; mails, so that will be de default.
(defun nsv/gnus-use-thunderbird-reply ()
  "Set `message-cite-style' equal to `message-cite-style-thunderbird' and reset message buffer."
  (interactive)
  (set (make-local-variable 'message-cite-style)
       message-cite-style-thunderbird)
  (delete-region (point) (point-max))
  (message-yank-original))

;; C-c C-s is kind of a lousy key in message-mode-map. Rebind it to
;; our function.
(define-key message-mode-map (kbd "C-c C-s") 'nsv/gnus-use-thunderbird-reply)

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

;; Invoke gnus with C-c m
(global-set-key (kbd "C-c m") 'gnus)

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

(add-to-list 'auto-mode-alist '(".offlineimaprc" . conf-mode))

(provide 'setup-mail)
;;; setup-mail.el ends here
