;;; ruby-programming.el --- Ruby related stuff.
;;; Commentary:
;;; Code:

;; The first thing is to automatically add end to any starting
;; sequence like def, if, while, do, etc.
(use-package ruby-end
  :ensure t
  :diminish ""
  :config
  (setq ruby-end-insert-newline nil))

;; yari. Docs are cool. Need to pacman -S ruby-docs for it to work.
(use-package yari
  :ensure t)

;; rails.vim really changed the way I work on rails projects, so let's
;; make something similiar with projectile-rails
(use-package projectile-rails
  :ensure t
  :diminish ""
  :init
  (projectile-rails-global-mode)
  :config
  ;; Use this as vim would. Not a lot of keybindings, instead remember
  ;; the names
  (evil-ex-define-cmd "Emodel"      'projectile-rails-find-model)
  (evil-ex-define-cmd "ECmodel"     'projectile-rails-find-current-model)
  (evil-ex-define-cmd "Econtroller" 'projectile-rails-find-controller)
  (evil-ex-define-cmd "ECcontroller" 'projectile-rails-find-current-controller)
  (evil-ex-define-cmd "Eview"       'projectile-rails-find-view)
  (evil-ex-define-cmd "ECview"      'projectile-rails-find-current-view)
  (evil-ex-define-cmd "Ehelper"     'projectile-rails-find-helper)
  (evil-ex-define-cmd "EChelper"    'projectile-rails-find-current-helper)
  (evil-ex-define-cmd "Elib"        'projectile-rails-find-lib)
  (evil-ex-define-cmd "Efeature"    'projectile-rails-find-feature)
  (evil-ex-define-cmd "Espec"       'projectile-rails-find-spec)
  (evil-ex-define-cmd "ECspec"      'projectile-rails-find-current-spec)
  (evil-ex-define-cmd "Etest"       'projectile-rails-find-test)
  (evil-ex-define-cmd "ECtest"      'projectile-rails-find-current-test)
  (evil-ex-define-cmd "Emigration"  'projectile-rails-find-migration)
  (evil-ex-define-cmd "ECmigration" 'projectile-rails-find-current-migration)
  (evil-ex-define-cmd "Efixture"    'projectile-rails-find-fixture)
  (evil-ex-define-cmd "ECfixture"   'projectile-rails-find-current-fixture)
  (evil-ex-define-cmd "Ejavascript" 'projectile-rails-find-javascript)
  (evil-ex-define-cmd "Estylesheet" 'projectile-rails-find-stylesheet)
  (evil-ex-define-cmd "Elog"        'projectile-rails-find-log)
  (evil-ex-define-cmd "Einitializer" 'projectile-rails-find-initializer)
  (evil-ex-define-cmd "Eenv"        'projectile-rails-find-environment)
  (evil-ex-define-cmd "Elocale"     'projectile-rails-find-locale)
  (evil-ex-define-cmd "Emailer"     'projectile-rails-find-mailer)
  (evil-ex-define-cmd "Elayout" 'projectile-rails-find-layout)
  )

;; Autocompletion. I'll need to test this
(use-package robe
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-robe)))

(provide 'init-ruby)
;;; init-ruby.el ends here
