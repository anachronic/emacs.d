;;; setup-modeline.el --- Mode line configurations
;;; Commentary:
;;; Code:

(defvar ach-valid-pyvenv-modes
  '(python-mode
    projectile-django-server-mode
    projectile-django-migration-mode
    web-mode
    js2-mode
    js2-jsx-mode)
  "Valid modes where we should display the current virtual environment.")

;; Will be trying spaceline for mode line.
(use-package spaceline
  :ensure t
  :demand)

;; No need to put this up top since we're demanding spaceline anyway
;; I'm gonna be following http://amitp.blogspot.cl/2017/01/emacs-spaceline-mode-line.html
;; for this conf
(require 'spaceline-config)
(require 'spaceline)

(spaceline-define-segment ach/narrow
  "Display Narrowed when buffer is narrowed."
  (when (buffer-narrowed-p)
    "Narrowed"))
(setq spaceline-minor-modes-separator "")
(setq powerline-default-separator 'bar)

;; Redefining my own theme
(defun spaceline--ach-theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-compile
    `(,left
      auto-compile
      ,second-left
      (major-mode :priority 79)
      (process :when active)
      ((flycheck-error flycheck-warning)
       :when active
       :priority 89)
      (minor-modes :when active
                   :priority 9)
      (version-control :when active
                       :priority 78)
      )
    `(((point-position
        line-column)
       :separator " | "
       :priority 96)
      (global :when active)
      ,@additional-segments
      (buffer-position :priority 99)
      (hud :priority 99)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(defun spaceline-ach-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.
ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'spaceline--ach-theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :face highlight-face
           :priority 100)
         ;should work something out with eyebrowse
         '((buffer-modified buffer-id remote-host)
           :priority 98)
         additional-segments))

(spaceline-ach-theme 'ach/narrow)


(provide 'setup-modeline)
;;; setup-modeline.el ends here
