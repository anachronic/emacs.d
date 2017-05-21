(setq gnus-select-method
      '(nnimap "Mail"
	       (nnimap-address "localhost")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-stream network)
               (nnimap-authenticator login)))

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %2{%-15,15f%}  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")
