;;; setup-docviews.el --- Image, PDF, svg, and etc setup.
;;; Commentary:
;;; Code:

(defun ach-evince-document ()
  "Open current document with Evince."
  (interactive)
  (let* ((file (f-this-file)))
    (call-process "evince" nil 0 nil file)))

(with-eval-after-load 'doc-view-minor-mode
  (define-key doc-view-mode-map (kbd "C-c C-v") 'ach-evince-document))


(provide 'setup-docviews)
;;; setup-docviews.el ends here
