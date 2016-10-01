;;; setup-recentf --- Setup Recentf.
;;; Commentary:
;;; Code:
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Let's define a blacklist

(defvar my/recentf-blacklist)
(setq my/recentf-blacklist
      '("company-statistics-cache.el"))

;; we need the filter function!!
(require 'dash)

;; I really haven't coded serious lisp in ages...
(mapc
 (lambda (banned-elem)
   (setq recentf-list
	 (-filter (lambda (total-elem)
		    (not (string-match-p banned-elem total-elem)))
		  recentf-list)))
 my/recentf-blacklist)


(global-set-key (kbd "C-S-x C-S-f") 'helm-recentf)

(provide 'setup-recentf)
;;; setup-recentf.el ends here