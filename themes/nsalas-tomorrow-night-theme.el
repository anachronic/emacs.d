;;; nsalas-tomorrow-night.el --- A custom theme forked from
;;; avk-darkblue trying to mimic sanityinc-tomorrow-night.
;;; Commentary:
;;; Code:

(deftheme nsalas-tomorrow-night
  "A dark theme forked from avk but mimicing sanityinc's tomorrow-night")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   `nsalas-tomorrow-night
   `(ac-candidate-face ((t (:background "gray23" :foreground "yellow2" :weight bold))))
   `(ac-selection-face ((t (:background "royalBlue4" :foreground "yellow2" :weight bold))))
   `(ahg-status-deleted-face ((default (:inherit font-lock-warning-face)) (nil nil)))
   `(ahg-status-modified-face ((default (:foreground "forest green" :inherit nil)) (nil nil)))
   `(ahg-status-unknown-face ((default (:foreground "yellow3")) (nil nil)))
   `(ahg-invisible-face ((default (:foreground "gray23")) (nil nil)))
   `(anything-ff-directory ((t (:inherit dired-directory))))
   `(anything-header ((t (:inherit font-lock-function-name-face))))
   `(bm-face ((t (:background "#460000" :foreground "white" :box (:line-width 2 :color "#460000" :style released-button) :weight bold))))
   `(comint-highlight-input ((t (:foreground "LightSkyBlue1" :weight bold))))

   ;; company
   `(company-tooltip ((t (:background "#444" :foreground "white"))))
   `(company-tooltip-selection ((t (:weight bold :background "#2f4f4f"))))
   `(company-tooltip-common-selection ((t (:weight bold))))
   `(company-scrollbar-bg ((t (:background "dim gray"))))
   `(company-scrollbar-fg ((t (:background "silver"))))
   `(company-tooltip-common ((t (:foreground "#00fa9a"))))
   `(company-tooltip-annotation ((t (:foreground "#Fa8072"))))

   ;; compilation
   `(compilation-error ((t (:inherit font-lock-warning-face))))
   `(compilation-line-number ((t (:foreground "orange red" :weight bold))))
   `(cursor ((t (:background "#cc6666"))))
   `(variable-pitch ((t (:family "Sans"))))
   `(custom-variable-tag ((((class color) (background dark)) (:inherit variable-pitch :foreground "DarkOrange" :weight bold))))
   `(default ((t (:stipple nil :background "#1d1f21" :foreground "#dddddd" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal))))
   `(diff-added ((t (:foreground "lawn green"))))
   `(diff-removed ((t (:foreground "light coral"))))

   ;; dired
   `(dired-directory ((t (:inherit diredp-dir-name))))
   `(dired-header ((t (:inherit font-lock-type-face :weight bold :height 1.2 :family "verdana"))))
   `(dired-mark ((t (:inherit font-lock-constant-face :foreground "VioletRed4"))))
   `(dired-marked ((t (:background "VioletRed4" :underline "green"))))
   `(dired-k-directory ((t (:foreground "#00bfff" :weight bold))))
   `(diredp-dir-name ((t (:foreground "#Eedd82" :weight bold))))
   `(diredp-dir-heading ((t (:foreground "#00ffff" :weight bold))))
   `(diredp-ignored-file-name ((t (:foreground "#696969"))))
   `(diredp-read-priv ((t (:foreground "#b5bd68"))))
   `(diredp-write-priv ((t (:foreground "#f0c674"))))
   `(diredp-exec-priv ((t (:foreground "#de935f"))))
   `(diredp-no-priv ((t (:inherit default))))
   `(diredp-deletion-file-name ((t (:foreground "#bc0101" :strike-through t))))
   `(diredp-deletion ((t (:foreground "#Eedd82" :background "#c40000" :weight bold))))
   `(diredp-file-name ((t (:foreground "white"))))
   `(diredp-compressed-file-suffix ((t (:foreground "#4682b4"))))
   `(diredp-file-suffix ((t (:foreground "MediumSeaGreen"))))
   `(diredp-number ((t (:foreground "yellow3"))))
   `(diredp-number ((t (:foreground "DodgerBlue2" :weight bold))))


   `(django-tag-face ((t (:background "#092e20" :box (:line-width 1 :color "grey22")))) t)
   `(django-variable-face ((t (:foreground "#479dcc"))) t)
   `(dropdown-list-face ((t (:inherit default :background "gray89" :foreground "black"))))
   `(dropdown-list-selection-face ((t (:inherit dropdown-list :background "#6d1717"))))
   `(ecb-analyse-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "DarkSlateBlue" :foreground "white"))))
   `(ecb-default-highlight-face ((((class color) (background dark)) (:background "DarkSlateBlue" :foreground "white"))))
   `(ecb-source-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "DeepSkyBlue" :foreground "white"))))
   `(ecb-tag-header-face ((((class color) (background dark)) (:background "SeaGreen1" :foreground "black"))))
   `(flymake-errline ((((class color)) (:background "#9d061f"))))
   `(flymake-infoline ((t (:background "gray20"))))
   `(flymake-warnline ((t (:background "#3f3f3f"))))
   `(flycheck-error ((t (:background "brown4"))))
   `(flycheck-fringe-error ((t (:inherit (flycheck-error)))))
   `(font-lock-builtin-face ((t (:foreground "#c58fe0"))))
   `(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face)) (((class color) (min-colors 16)) nil)))
   `(font-lock-comment-face ((t (:foreground "#696969" :slant italic :weight normal))))
   `(font-lock-function-name-face ((t (:foreground "#00dddd" :weight bold))))
   `(font-lock-keyword-face ((t (:foreground "darkorange"))))
   `(font-lock-string-face ((t (:foreground "#8abeb7"))))
   `(font-lock-type-face ((t (:foreground "#b5bd68"))))
   `(font-lock-doc-face ((t (:foreground "#765587"))))
   `(font-lock-variable-name-face ((t (:foreground "#6897d1"))))
   `(font-lock-constant-face ((t (:foreground "#81a2be"))))
   `(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:foreground "PaleVioletRed3" :weight bold))))

   ;; hl-symbol
   `(highlight-symbol-face ((t (:background "Darkslategray"))));;DarkSlateGray

   ;; Gnus
   `(gnus-summary-normal-ancient ((t (:foreground "grey" :background nil))))
   `(gnus-summary-normal-read ((t (:foreground "PaleGreen" :background nil))))
   `(gnus-summary-normal-unread ((t (:foreground "orchid1" :background nil))))
   `(gnus-cite-1 ((t (:foreground "CadetBlue1" :background nil))))
   `(gnus-cite-2 ((t (:foreground "burlywood1" :background nil))))
   `(gnus-cite-3 ((t (:foreground "PaleGreen" :background nil))))
   `(gnus-group-mail-3 ((t (:foreground "DeepSkyBlue" :weight bold :background nil))))
   `(gnus-group-mail-3-empty ((t (:foreground "MediumTurquoise" :background nil))))


   ;; Helm
   `(helm-buffer-saved-out ((t (:foreground "red"))))
   `(helm-buffer-directory ((t (:inherit helm-ff-dotted-directory :weight ultra-light))))
   `(helm-dired-directory ((t (:inherit dired-directory))))
   `(helm-grep-file ((t (:foreground "#329ba3" :weight bold))))
   `(helm-header ((t (:foreground "LightGreen" :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground "gold3" :weight bold))))
   `(helm-ff-invalid-symlink ((t (:inherit dired-symlink :strike-through "red"))))
   `(helm-ff-executable ((t (:foreground "green" :weight bold))))
   `(helm-ff-directory ((t (:inherit helm-ff-dotted-directory))))
   `(helm-ff-symlink ((t (:inherit dired-symlink))))
   `(helm-ff-file ((t (:foreground "#5f9ea0"))))
   `(helm-selection ((t (:foreground "Navy" :underline t :weight bold :background "DeepSkyBlue"))))
   `(helm-source-header ((t (:background "midnight blue" :foreground "gray77" :box (:line-width 2 :color "black" :style pressed-button) :weight bold))))
   `(helm-bookmark-file ((t (:foreground "#06c950"))))
   `(helm-visible-mark ((t (:background "dark green"))))

   ;; ivy faces
   `(ivy-virtual ((t (:foreground "LightGoldenrod"))))
   `(ivy-current-match ((t (:foreground "Navy" :underline t :weight bold :background "DeepSkyBlue"))))
   `(ivy-minibuffer-match-face-1 ((t (:weight bold :foreground "IndianRed"))))
   `(ivy-minibuffer-match-face-2 ((t (:weight bold :foreground "DarkSalmon"))))
   `(ivy-minibuffer-match-face-3 ((t (:weight bold :foreground "yellow2"))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit default))))

   ;; misc..
   `(highlight ((t (:weight normal :background "gray6"))))
   `(highlight-changes ((((min-colors 88) (class color)) (:background "#00254a"))))
   `(highlight-changes-delete ((((min-colors 88) (class color)) (:background "#8b6969"))))
   `(hl-line ((t (:background "#0b3540"))))
   `(ido-first-match ((t (:background "#193980" :foreground "gray66" :weight bold))))
   `(ido-only-match ((t (:background "#15572c" :foreground "gray75" :weight bold))))
   `(ido-subdir ((((min-colors 88) (class color)) (:weight bold :foreground nil))))
   `(isearch ((t (:background "orange" :foreground "black"))))
   `(anzu-replace-highlight ((t (:background "#ffbfcc" :foreground "#B22222"))))
   `(anzu-replace-to ((t (:foreground "yellow" :underline t))))
   `(jabber-chat-prompt-foreign ((t (:inherit custom-face-tag))))
   `(jabber-chat-prompt-local ((t (:inherit org-level-4))))
   `(jabber-roster-user-online ((t (:inherit font-lock-string-face))))

   ;; latex
   `(font-latex-sectioning-2-face ((t (:foreground "yellow" :weight bold :underline t))))
   `(font-latex-sectioning-3-face ((t (:foreground "yellow" :weight bold :underline t))))
   `(font-latex-sectioning-4-face ((t (:foreground "yellow" :weight bold :underline t))))


   `(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "#483d8b" :foreground "white"))))
   `(link ((t (:foreground "#349b98" :underline t))))
   `(match ((((class color) (min-colors 88) (background dark)) (:background "SlateBlue4" :foreground "white"))))

   ;; Magit
   `(magit-section-highlight ((t (:background "grey20"))))
   `(magit-diff-context ((t (:foreground "grey70"))))
   `(magit-diff-context-highlight ((t (:background "grey20" :foreground "grey70"))))
   `(magit-diff-added ((t (:background "SpringGreen4" :foreground "#ddd"))))
   `(magit-diff-added-highlight ((t (:background "SeaGreen" :foreground "#cceecc"))))
   `(magit-diff-removed ((t (:background "brown" :foreground "#ffdddd"))))
   `(magit-diff-removed-highlight ((t (:background "IndianRed4" :foreground "#eecccc"))))


   ;; Messages
   `(message-cited-text ((((class color) (background dark)) (:foreground "SandyBrown"))))
   `(message-header-name ((((class color) (background dark)) (:foreground "DarkGrey"))))
   `(message-header-other ((((class color) (background dark)) (:foreground "LightPink2"))))
   `(message-header-subject ((((class color) (background dark)) (:foreground "yellow2"))))
   `(message-separator ((((class color) (background dark)) (:foreground "thistle"))))

   ;; Minibuffer prompt. heh
   `(minibuffer-prompt ((t (:foreground "gold2" :weight bold))))

   ;; Mode line
   `(mode-line ((t (:inverse-video nil :background "DodgerBlue4" :foreground "gray"))))
   `(mode-line-inactive ((t (:weight light :foreground "gray90" :background "grey31" :inherit (mode-line)))))
   `(mode-line-read-only-face ((t (:foreground "cyan3"))))
   `(mode-line-modified-face ((t (:foreground "white smoke" :background "#007400"))))
   `(mode-line-folder-face ((t (:foreground "gray60"))))
   `(mode-line-filename-face ((t (:foreground "#eab700" :weight bold))))
   `(mode-line-buffer-id ((t (:foreground "#eab700" :weight bold))))
   `(mode-line-position-face ((t (:family "Menlo" ))))
   `(mode-line-mode-face ((t (:foreground "gray80"))))
   `(mode-line-minor-mode-face ((t (:foreground "gray40"))))
   `(mode-line-process-face ((t :(foreground "LimeGreen"))))
   `(mode-line-80col-face ((t (mode-line-position-face :foreground "black" :background "#eab700"))))

   ;; MuMaMo -- I don't really use this
   `(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "#123a4d"))))
   `(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "#2e2e51"))))

   ;; notmuch
   `(notmuch-hello-logo-background ((t (:background "#2f4f4f"))))
   `(notmuch-message-summary-face ((t (:inherit font-lock-function-name-face))))
   `(notmuch-search-count ((t (:inherit default :foreground "yellow3"))))
   `(notmuch-search-date ((t (:inherit default :foreground "SpringGreen4"))))
   `(notmuch-search-subject ((t (:foreground "dark turquoise"))))
   `(notmuch-search-unread-face ((t (:weight bold))))

   ;; org mode
   `(org-agenda-clocking ((t (:inherit secondary-selection))) t)
   `(org-agenda-date-today ((t (:foreground "PaleVioletRed1" :weight bold))))
   `(org-agenda-structure ((t (:foreground "DimGrey" :weight bold))))
   `(org-archived ((t (:foreground "LemonChiffon4"))))
   `(org-column ((t (:weight normal :slant normal :inherit default))))
   `(org-checkbox ((t (:inherit font-lock-function-name-face))))
   `(org-column-title ((t (:background "dark green" :underline t :weight bold))))
   `(org-date ((t (:foreground "dark orange" :underline t))))
   `(org-document-title ((t (:foreground "PowderBlue" :weight bold))))
   `(org-habit-alert-face ((((background dark)) (:background "gold" :foreground "red3"))))
   `(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "dim gray" :strike-through t))))
   `(org-hide ((((background dark)) (:foreground "darkslategrey"))))
   `(outline-1 ((t (:foreground "LightSteelBlue2" :inherit nil))))
   `(org-level-1 ((t (:foreground "yellow" :weight bold))))
   `(org-level-2 ((t (:foreground "#14d169" :weight bold))))
   `(org-level-3 ((t (:foreground "#e59dea" :weight bold))))
   `(org-level-4 ((t (:foreground "LightCoral" :weight bold))))
   `(org-link ((t (:foreground "#329ba3" :underline "IndianRed3" :weight bold))))
   `(org-mode-line-clock ((t (:inherit modeline))) t)
   `(org-scheduled ((t (:foreground "CadetBlue1" :slant italic))))
   `(org-todo ((t (:foreground "#Cd5c5c" :weight bold))))
   `(org-done ((t (:foreground "DodgerBlue" :weight bold))))
   `(org-tag ((t (:foreground "#20b2aa" :height 105 :weight semi-light))))
   `(org-scheduled-previously ((t (:foreground "indian red"))))
   `(org-scheduled-today ((t (:foreground "SeaGreen3" :slant italic))))
   `(org-special-keyword ((t (:foreground "#9370db"))))

   `(org-upcoming-deadline ((t (:foreground "hot pink"))))
   `(outline-3 ((t (:foreground "seashell3" :weight bold))))
   `(outline-7 ((t (:foreground "chartreuse"))))
   `(py-XXX-tag-face ((t (:background "medium violet red" :foreground "white"))) t)
   `(py-builtins-face ((t (:foreground "medium sea green"))) t)
   `(py-class-name-face ((t (:foreground "deep sky blue"))) t)
   `(py-decorators-face ((t (:foreground "cyan4"))) t)
   `(py-pseudo-keyword-face ((t (:foreground "RosyBrown3"))) t)
   `(persp-selected-face ((t (:foreground "#48d1cc" :weight bold))))
   `(region ((t (:background "#5b1737" :foreground "white"))))
   `(rst-level-1-face ((t (:background "SlateBlue4"))) t)
   `(rst-level-2-face ((t (:background "grey20"))) t)
   `(secondary-selection ((t (:background "forest green" :foreground "white"))))
   `(shadow ((((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey40"))))
   `(show-paren-match ((t (:underline t :weight bold :inverse-video t))))
   `(smerge-refined-change ((t (:background "midnight blue"))))
   `(tabbar-default ((t (:inherit variable-pitch :background "gray75" :foreground "MidNightBlue" :family "verdana"))))
   `(tabbar-selected ((t (:inherit tabbar-default :foreground "red4" :family "verdana"))))
   `(tooltip ((((class color)) (:inherit variable-pitch :background "IndianRed1" :foreground "black"))))
   `(trailing-whitespace ((t (:background "#182749"))))
   `(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "dark red"))))
   `(whitespace-empty ((t (:background "gray33"))))
   `(whitespace-line ((t (:foreground "DarkOrange1"))))
   `(whitespace-space ((((class color) (background dark)) (:foreground "gray40"))))
   `(whitespace-tab ((t (:background "#03222f"))))
   `(widget-button ((t (:foreground "yellow2"))))
   `(fringe ((t (:background "#1d1f21" :foreground "yellow"))))

   ;; ediff
   `(ediff-current-diff-A ((t (:background "pale green" :foreground "firebrick"))))
   `(ediff-current-diff-Ancestor ((t (:background "VioletRed" :foreground "Black"))))
   `(ediff-current-diff-B ((t (:background "Yellow" :foreground "DarkOrchid"))))
   `(ediff-current-diff-C ((t (:background "Pink" :foreground "Navy"))))
   `(ediff-even-diff-A ((t (:background "light grey" :foreground "Black"))))
   `(ediff-even-diff-Ancestor ((t (:background "Grey" :foreground "White"))))
   `(ediff-even-diff-B ((t (:background "Grey" :foreground "White"))))
   `(ediff-even-diff-C ((t (:background "light grey" :foreground "Black"))))
   `(ediff-fine-diff-A ((t (:background "sky blue" :foreground "Navy"))))
   `(ediff-fine-diff-Ancestor ((t (:background "Green" :foreground "Black"))))
   `(ediff-fine-diff-B ((t (:background "cyan" :foreground "Black"))))
   `(ediff-fine-diff-C ((t (:background "Turquoise" :foreground "Black"))))
   `(ediff-odd-diff-A ((t (:background "Grey" :foreground "White"))))
   `(ediff-odd-diff-Ancestor ((t (:background "gray40" :foreground "cyan3"))))
   `(ediff-odd-diff-B ((t (:background "light grey" :foreground "Black"))))
   `(ediff-odd-diff-C ((t (:background "Grey" :foreground "White"))))

   ;; web mode
   `(web-mode-block-control-face ((t (:foreground "#86caf7"))))
   `(web-mode-block-delimiter-face ((t (:foreground "#4e78f5"))))
   `(web-mode-html-tag-face ((t (:foreground "#de835f"))))
   `(web-mode-html-tag-bracket-face ((t (:foreground "white"))))
   `(web-mode-function-call-face ((t (:foreground "#e9aafa"))))
   `(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face))))
   `(web-mode-html-attr-name-face ((t (:foreground "#f0c674"))))
   `(web-mode-javascript-string-face ((t (:inherit font-lock-string-face))))
   `(web-mode-current-element-highlight-face ((t (:foreground "white" :background "DarkSlateGray" :weight bold))))

   ;; ERC
   `(erc-default-face ((t (:foreground "white"))))
   `(erc-nick-default-face ((t (:foreground "Goldenrod" :weight bold))))
   `(erc-button ((t (:foreground "DeepSkyBlue" :underline t :weight bold))))
   `(erc-notice-face ((t (:foreground "DarkSlateBlue"))))
   `(erc-timestamp-face ((t (:foreground "orange red" :weight bold))))
   `(erc-input-face ((t (:foreground "DarkTurquoise"))))

   ;; Hydra faces
   `(hydra-face-red ((t (:foreground "#f44256" :weight bold))))
   `(hydra-face-blue ((t (:foreground "#4b9af4" :weight bold))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground "deep pink"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "chartreuse"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "deep sky blue"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "orchid"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "spring green"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "sienna1"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "dark orange"))))))

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nsalas-tomorrow-night)
;;; nsalas-tomorrow-night.el ends here
