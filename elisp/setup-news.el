;;; setup-news.el --- Set up RSS configuration and feeds.
;;; Commentary:
;;; Code:

;; Let's read some news, shall we?
;; Note that this does NOT ensure elfeed is installed
(use-package elfeed
  :commands elfeed
  :bind ("C-x w" . elfeed)
  :config
  (progn
    (add-hook 'elfeed-show-mode-hook
              (lambda ()
                (progn
                  (setq visual-fill-column-width 120)
                  (visual-fill-column-mode))))))


;; configure our news feeds.
(setq elfeed-feeds
      '(("http://emacsredux.com/atom.xml" emacs)
	("http://nullprogram.com/feed/" emacs)
	("https://www.schneier.com/blog/atom.xml" security)
	("http://www.muylinux.com/feed/" linux)
	("http://feeds.feedburner.com/LinuxAdictos" linux)
	("http://feeds.arstechnica.com/arstechnica/technology-lab?format=xml" tech)))




(provide 'setup-news)
;;; setup-news.el ends here
