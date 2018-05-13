;;; setup-evil-fixes.el --- Nothing substantial about this file..
;;; Commentary:
;;; Code:

;; First: package menu
(defun evil-collection-package-menu-setup ()
  "Set up `evil' bindings for `package-menu'."
  (evil-set-initial-state 'package-menu-mode 'normal)

  (evil-define-key 'normal package-menu-mode-map
    "i" 'package-menu-mark-install
    "U" 'package-menu-mark-upgrades
    "d" 'package-menu-mark-delete

    ;; undo
    "u" 'package-menu-mark-unmark

    ;; execute
    "x" 'package-menu-execute

    "q" 'quit-window ;; FIXME: Can macros make sense here?
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))

(add-hook 'package-menu-mode-hook 'evil-collection-package-menu-setup)

(provide 'setup-evil-fixes)
;;; setup-evil-fixes.el ends here
