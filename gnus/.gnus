(setq gnus-select-method
      '(nnimap "Mail"
	       (nnimap-address "localhost")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-stream network)
               (nnimap-authenticator login)))

;; Formatting
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %2{%-15,15f%}  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

;; More setq-defaults
(setq-default gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))

;; I'd rather not see rich text or html if it's possible
;; source:
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/FAQ-4_002d6.html#FAQ-4_002d6
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; We like gnus topics, don't we? From
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Topics.html
;; And, of course, John Wiegley and Sacha Chua chat. It's wonderful.
;; https://www.youtube.com/watch?v=nUjgKoOYxos
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Customizations
(setq gnus-permanently-visible-groups "INBOX")
