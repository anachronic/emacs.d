;;; setup-blogging.el --- Blogging packages.
;;; Commentary:
;;; Code:

;; I was using jekyll before, but hexo looks better
(use-package hexo
  :ensure t
  :commands (hexo hexo-new)
  :defer t
  :config
  (setq hexo-new-format 'org))

;; (require 'hexo)
;; (setq hexo-new-format 'org)

(provide 'setup-blogging)
;;; setup-blogging.el ends here
