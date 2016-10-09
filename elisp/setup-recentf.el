;;; setup-recentf --- Setup Recentf.
;;; Commentary:
;;; Code:
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 35)

;; Let's define a blacklist

(defvar my/recentf-blacklist)
(setq my/recentf-blacklist
      '("company-statistics-cache.el"
        "-autoloads.el"
        ".elfeed/index"))

;; we need the filter function!!
(use-package dash
  :ensure t)

;; I really haven't coded serious lisp in ages...
;; Get rid of the files we don't want to see in the recentf list
(require 'dash)

(defun my/recentf-ban-from-blacklist ()
  "Remove all matching substrings in my/recentf-blacklist from recentf."
  (mapc
   (lambda (banned-elem)
     (setq recentf-list
           (-filter (lambda (total-elem)
                      (not (string-match-p banned-elem total-elem)))
                    recentf-list)))
   my/recentf-blacklist))

(my/recentf-ban-from-blacklist)

(global-set-key (kbd "C-S-x C-S-f") 'helm-recentf)

(add-hook 'kill-emacs-hook 'my/recentf-ban-from-blacklist)

(provide 'setup-recentf)
;;; setup-recentf.el ends here
