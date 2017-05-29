;;; setup-keymaps.el --- Custom keymaps and macros associated.
;;; Commentary:
;;; Code:

;; I'd like to have a cool prefix key to do my own things without
;; having to worry that anyone overrides its key binding and, as you
;; know, C-c is quite cluttered. So, let's rebind C-a to combine M-m
;; and C-a and voilà!, we get a free cool key to bind to a prefix.
;; The rebinding happens in setup-editor.el
(global-unset-key (kbd "M-m"))
(define-prefix-command 'meta-m-map)
(global-set-key (kbd "M-m") 'meta-m-map)

;; There are some files I like to jump to. C-x j seems like a sensible
;; prefix for this.
(define-prefix-command 'file-bookmark-map)
(define-key ctl-x-map (kbd "j") 'file-bookmark-map)

(require 'f)
(defmacro nsv/define-bookmark (file key &optional name)
  "Define a bookmark to FILE with KEY in `file-bookmark-map' and give it NAME.

If NAME is not present, infer from FILE instead."
  (declare (indent defun))
  (let* ((filename (f-filename (eval file)))
         (bmk-name (if name name (f-no-ext filename)))
         (funname (format "bookmark/%s" bmk-name)))
    (eval `(defun ,(intern funname) ()
             ,(format "Jump to %s directly." (eval file))
             (interactive)
             (find-file (file-truename ,file))))
    (eval `(define-key file-bookmark-map (kbd ,key) (quote ,(intern funname))))
    t))


(provide 'setup-keymaps)
;;; setup-keymaps.el ends here
