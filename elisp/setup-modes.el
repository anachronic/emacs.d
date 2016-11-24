;;; setup-modes.el --- Modes that don't require separate files.
;;; Commentary:
;;; Code:

;; I used to have a prefix for the mode toggle, but I think hydras can be very useful, because the help can be displayed vertically.
(defhydra mode-toggle (:color blue
                       :columns 1)
  "Toggle one of these modes"
  ("e" evil-mode "Evil mode")
  ("w" whitespace-mode "Whitespace mode")
  ("l" linum-relative-mode "Linum relative")
  ("c" color-identifiers-mode "Color identifiers mode")
  ("v" visual-line-mode "Visual line mode")
  ("p" projectile-mode "Projectile")
  ("k" which-key-mode "Which key")
  ("h" global-hl-line-mode "Highlight line")
  ("a" artist-mode "Artist mode")
  ("s" key-chord-mode "Key chord mode")
  ("q" nil "quit"))

(global-set-key (kbd "<f6>") 'mode-toggle/body)

;; yaml-mode. mainly for syntax highlighting
(use-package yaml-mode
  :ensure t ;; seems like overkill
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'setup-modes)
;;; setup-modes.el ends here
