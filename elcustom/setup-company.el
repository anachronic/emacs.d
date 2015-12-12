(global-company-mode)

(defun my/remove-semantic ()
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-c-headers))

(add-hook 'c-mode-hook 'my/remove-semantic)
(add-hook 'c++-mode-hook 'my/remove-semantic)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map [tab] 'company-complete-common-or-cycle)))
;; (setq company-selection t) <-- this doesn't make sense -- Dmitry
(setq company-idle-delay 0)

(provide 'setup-company)
