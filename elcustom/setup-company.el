(global-company-mode)

(defun my/company-add-c-headers ()
  (add-to-list 'company-backends 'company-c-headers))

(add-hook 'c-mode-hook 'my/company-add-c-headers)
(add-hook 'c++-mode-hook 'my/company-add-c-headers)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map [tab] 'company-complete-common-or-cycle)))
;; (setq company-selection t) <-- this doesn't make sense -- Dmitry
(setq company-idle-delay 0.3)
(company-statistics-mode)

(provide 'setup-company)
