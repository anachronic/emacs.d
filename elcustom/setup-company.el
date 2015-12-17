(global-company-mode)

(defun my/custom-backends ()
  (setq company-backends (delete 'company-capf company-backend))
  (add-to-list 'company-backends 'company-c-headers))

(setq-default company-backends
	      '((company-semantic
		 company-bbdb
		 company-nxml
		 company-css
		 company-eclim
		 company-semantic
		 company-clang
		 company-xcode
		 company-cmake
		 company-dabbrev-code
		 company-gtags
		 company-etags
		 company-keywords
		 company-oddmuse
		 company-files
		 company-capf
		 company-c-headers)))

;(add-hook 'c-mode-hook 'my/custom-backends)
;(add-hook 'c++-mode-hook 'my/custom-backends)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map [tab] 'company-complete-common-or-cycle)))
;; (setq company-selection t) <-- this doesn't make sense -- Dmitry
(setq company-idle-delay 0)

(provide 'setup-company)
