;;; setup-recentf --- Setup Recentf.
;;; Commentary:
;;; Code:
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 100)


;; Let's define a blacklist

(defvar ach-recentf-blacklist)
(setq ach-recentf-blacklist
      '("company-statistics-cache.el"
        "-autoloads.el"
        ".elfeed/index"))

;; I really haven't coded serious lisp in ages...
;; Get rid of the files we don't want to see in the recentf list
(require 'dash)

(defun ach-recentf-ban-from-blacklist ()
  "Remove all matching substrings in ach-recentf-blacklist from recentf."
  (mapc
   (lambda (banned-elem)
     (setq recentf-list
           (-filter (lambda (total-elem)
                      (not (string-match-p banned-elem total-elem)))
                    recentf-list)))
   ach-recentf-blacklist))

(ach-recentf-ban-from-blacklist)

(add-hook 'kill-emacs-hook 'ach-recentf-ban-from-blacklist)

(provide 'setup-recentf)
;;; setup-recentf.el ends here
