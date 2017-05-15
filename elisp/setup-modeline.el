;;; setup-modeline.el --- Mode line configurations
;;; Commentary:
;;; Code:

(defvar nsv/valid-pyvenv-modes
  '(python-mode projectile-django-server-mode projectile-django-migration-mode)
  "Valid modes where we should display the current virtual environment.")

;; A customized mode line. For now it has pyvenv bindings. This was
;; *so* complicated. I got most of this from these pages:
;; https://github.com/TheBB/spaceline/blob/master/spaceline-segments.el
;; https://github.com/jorgenschaefer/Config/blob/master/emacs.el
;; https://emacs.stackexchange.com/questions/7863/show-colors-in-mode-line-for-minor-modes
;; Thanks everyone!
(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification

                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                (pyvenv-virtual-env-name
                 (:eval (list (if (and (member major-mode nsv/valid-pyvenv-modes)
                                       (bound-and-true-p pyvenv-virtual-env-name))
                                  (propertize (concat "[" pyvenv-virtual-env-name "] ")
                                              'face '(:foreground "SandyBrown"))
                                ""))))
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'setup-modeline)
;;; setup-modeline.el ends here