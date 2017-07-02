;;; c-programming.el --- Relevant stuff for coding in C.
;;; Commentary:
;;; Code:

;; I hate abbrev when coding c-like languages!!
(defun ach-abbrev-gtfo ()
  "Get rid of abbrev."
  (abbrev-mode -1))

(add-hook 'c-mode-common-hook 'ach-abbrev-gtfo)

;; I'll set the default style to k&r.
;; we might want to try out linux. We'll see
(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "k&r"))
              c-basic-offset 4)



(defun ach-add-header-jump ()
  "Add a header jump key binding."
  (local-set-key (kbd "C-c j h") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'ach-add-header-jump)

(provide 'init-c)
;;; init-c.el ends here
