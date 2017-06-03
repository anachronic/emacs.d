;;; setup-news.el --- Set up RSS configuration and feeds.
;;; Commentary:
;;; Code:

;; Elfeed. No key bound
(use-package elfeed
  :ensure t
  :commands elfeed
  :defer t
  :config
  (progn
    (add-hook 'elfeed-show-mode-hook
              (lambda ()
                (progn
                  (setq visual-fill-column-width 120)
                  (visual-fill-column-mode))))))


;; configure our news feeds.
(with-eval-after-load 'elfeed
  (setq elfeed-feeds
        '(("http://emacsredux.com/atom.xml" emacs)
          ("http://nullprogram.com/feed/" emacs)
          ("http://endlessparentheses.com/atom.xml" emacs)
          ("http://oremacs.com/atom.xml" emacs)
          ("https://www.masteringemacs.org/feed" emacs)
          ("https://www.schneier.com/blog/atom.xml" security)
          ("http://www.muylinux.com/feed/" linux)
          ("http://feeds.feedburner.com/LinuxAdictos" linux)))
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode))



(provide 'setup-news)
;;; setup-news.el ends here
