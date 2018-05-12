;;; c-programming.el --- Relevant stuff for coding in C.
;;; Commentary:
;;; Code:

;; I'll set the default style to k&r.
;; we might want to try out linux. We'll see
(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "k&r"))
              c-basic-offset 4)

(provide 'init-c)
;;; init-c.el ends here
