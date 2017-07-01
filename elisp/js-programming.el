;;; js-programming.el --- Javascript programming relevant stuff.
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :commands (js2-mode js2-jsx-mode)
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2)
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-idle-timer-delay 0.4)
  (add-hook 'js2-mode-hook (lambda () (color-identifiers-mode -1)))
  (with-eval-after-load 'dumb-jump
    (define-key js2-mode-map (kbd "M-.") 'dumb-jump-go)
    (define-key js2-mode-map (kbd "M-,") 'dumb-jump-back))

  ;; Set sensible mode lighters...
  (add-hook 'js2-mode-hook (lambda () (setq-local mode-name "Javascript")))
  (add-hook 'js2-jsx-mode-hook (lambda () (setq-local mode-name "JSX")))

  ;; The following comes from:
  ;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-javascript.el

  ;; Which comes from
  ;; https://emacs.stackexchange.com/a/3885/13735

  ;; Feature is imenu for Angular and I believe others
  (defvar javascript-common-imenu-regex-list
    '(("Attribute" " \\([a-z][a-zA-Z0-9-_]+\\) *= *\{[a-zA-Z0-9_.(), ]+\}\\( \\|$\\)" 1)
      ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
      ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
      ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
      ("State" "[. \t]state[(:][ \t]*['\"]\\([^'\"]+\\)" 1)
      ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
      ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
      ("Module" "[. \t]module( *['\"]\\([a-zA-Z0-9_.]+\\)['\"], *\\[" 1)
      ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
      ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
      ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
      ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
      ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
      ("Value" "[. \t]value([ \t]*['\"]\\([^'\"]+\\)" 1)
      ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
      ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
      ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
      ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
      ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
      ;; ;; {{ es6 beginning
      ;; ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" 1) ;; es6 fn1 () { }
      ;; ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*=[ \t]*(?[a-zA-Z0-9, ]*)?[ \t]*=>" 1) ;; es6 fn1 = (e) =>
      ;; ;; }}
      ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)
      ))

  ;; {{ patching imenu in js2-mode
  (setq js2-imenu-extra-generic-expression javascript-common-imenu-regex-list)

  (defvar js2-imenu-original-item-lines nil
    "List of line infomration of original imenu items.")

  (defun js2-imenu--get-line-start-end (pos)
    (let* (b e)
      (save-excursion
        (goto-char pos)
        (setq b (line-beginning-position))
        (setq e (line-end-position)))
      (list b e)))

  (defun js2-imenu--get-pos (item)
    (let* (val)
      (cond
       ((integerp item)
        (setq val item))

       ((markerp item)
        (setq val (marker-position item))))

      val))

  (defun js2-imenu--get-extra-item-pos (item)
    (let* (val)
      (cond
       ((integerp item)
        (setq val item))

       ((markerp item)
        (setq val (marker-position item)))

       ;; plist
       ((and (listp item) (listp (cdr item)))
        (setq val (js2-imenu--get-extra-item-pos (cadr item))))

       ;; alist
       ((and (listp item) (not (listp (cdr item))))
        (setq val (js2-imenu--get-extra-item-pos (cdr item)))))

      val))

  (defun js2-imenu--extract-line-info (item)
    "Recursively parse the original imenu items created by js2-mode.
The line numbers of items will be extracted."
    (let* (val)
      (if item
          (cond
           ;; Marker or line number
           ((setq val (js2-imenu--get-pos item))
            (push (js2-imenu--get-line-start-end val)
                  js2-imenu-original-item-lines))

           ;; The item is Alist, example: (hello . 163)
           ((and (listp item) (not (listp (cdr item))))
            (setq val (js2-imenu--get-pos (cdr item)))
            (if val (push (js2-imenu--get-line-start-end val)
                          js2-imenu-original-item-lines)))

           ;; The item is a Plist
           ((and (listp item) (listp (cdr item)))
            (js2-imenu--extract-line-info (cadr item))
            (js2-imenu--extract-line-info (cdr item)))

           ;;Error handling
           (t (message "Impossible to here! item=%s" item))))))

  (defun js2-imenu--item-exist (pos lines)
    "Try to detect does POS belong to some LINE"
    (let* (rlt)
      (dolist (line lines)
        (if (and (< pos (cadr line)) (>= pos (car line)))
            (setq rlt t)))
      rlt))

  (defun js2-imenu--is-item-already-created (item)
    (unless (js2-imenu--item-exist
             (js2-imenu--get-extra-item-pos item)
             js2-imenu-original-item-lines)
      item))

  (defun js2-imenu--check-single-item (r)
    (cond
     ((and (listp (cdr r)))
      (let (new-types)
        (setq new-types
              (delq nil (mapcar 'js2-imenu--is-item-already-created (cdr r))))
        (if new-types (setcdr r (delq nil new-types))
          (setq r nil))))
     (t (if (js2-imenu--item-exist (js2-imenu--get-extra-item-pos r)
                                   js2-imenu-original-item-lines)
            (setq r nil))))
    r)

  (defun my-validate-json-or-js-expression (&optional not-json-p)
    "Validate buffer or select region as JSON.
If NOT-JSON-P is not nil, validate as Javascript expression instead of JSON."
    (interactive "P")
    (let* ((json-exp (if (region-active-p) (my-selected-str)
                       (my-buffer-str)))
           (jsbuf-offet (if not-json-p 0 (length "var a=")))
           errs
           first-err
           (first-err-pos (if (region-active-p) (region-beginning) 0)))
      (unless not-json-p
        (setq json-exp (format "var a=%s;"  json-exp)))
      (with-temp-buffer
        (insert json-exp)
        (unless (featurep 'js2-mode)
          (require 'js2-mode))
        (js2-parse)
        (setq errs (js2-errors))
        (cond
         ((not errs)
          (message "NO error found. Good job!"))
         (t
          ;; yes, first error in buffer is the last element in errs
          (setq first-err (car (last errs)))
          (setq first-err-pos (+ first-err-pos (- (cadr first-err) jsbuf-offet)))
          (message "%d error(s), first at buffer position %d: %s"
                   (length errs)
                   first-err-pos
                   (js2-get-msg (caar first-err))))))
      (if first-err (goto-char first-err-pos))))

  (defun my-print-json-path (&optional hardcoded-array-index)
    "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
    (interactive "P")
    (cond
     ((memq major-mode '(js2-mode))
      (js2-print-json-path hardcoded-array-index))
     (t
      (let* ((cur-pos (point))
             (str (my-buffer-str)))
        (when (string= "json" (file-name-extension buffer-file-name))
          (setq str (format "var a=%s;" str))
          (setq cur-pos (+ cur-pos (length "var a="))))
        (unless (featurep 'js2-mode)
          (require 'js2-mode))
        (with-temp-buffer
          (insert str)
          (js2-init-scanner)
          (js2-do-parse)
          (goto-char cur-pos)
          (js2-print-json-path))))))

  (defun js2-imenu--remove-duplicate-items (extra-rlt)
    (delq nil (mapcar 'js2-imenu--check-single-item extra-rlt)))

  (defun js2-imenu--merge-imenu-items (rlt extra-rlt)
    "RLT contains imenu items created from AST.
EXTRA-RLT contains items parsed with simple regex.
Merge RLT and EXTRA-RLT, items in RLT has *higher* priority."
    ;; Clear the lines.
    (set (make-variable-buffer-local 'js2-imenu-original-item-lines) nil)
    ;; Analyze the original imenu items created from AST,
    ;; I only care about line number.
    (dolist (item rlt)
      (js2-imenu--extract-line-info item))

    ;; @see https://gist.github.com/redguardtoo/558ea0133daa72010b73#file-hello-js
    ;; EXTRA-RLT sample:
    ;; ((function ("hello" . #<marker 63>) ("bye" . #<marker 128>))
    ;;  (controller ("MyController" . #<marker 128))
    ;;  (hellworld . #<marker 161>))
    (setq extra-rlt (js2-imenu--remove-duplicate-items extra-rlt))
    (append rlt extra-rlt))

  ;; {{ print json path, will be removed when latest STABLE js2-mode released
  (defun js2-get-element-index-from-array-node (elem array-node &optional hardcoded-array-index)
    "Get index of ELEM from ARRAY-NODE or 0 and return it as string."
    (let* ((idx 0) elems (rlt hardcoded-array-index))
      (setq elems (js2-array-node-elems array-node))
      (if (and elem (not hardcoded-array-index))
          (setq rlt (catch 'nth-elt
                      (dolist (x elems)
                        ;; We know the ELEM does belong to ARRAY-NODE,
                        (if (eq elem x) (throw 'nth-elt idx))
                        (setq idx (1+ idx)))
                      0)))
      (format "[%s]" rlt)))
  ;; }}

  (eval-after-load 'js2-mode
    '(progn
       (defadvice js2-mode-create-imenu-index (around my-js2-mode-create-imenu-index activate)
         (let (rlt extra-rlt)
           ad-do-it
           (setq extra-rlt
                 (save-excursion
                   (imenu--generic-function js2-imenu-extra-generic-expression)))
           (setq ad-return-value (js2-imenu--merge-imenu-items ad-return-value extra-rlt))
           ad-return-value))))
  ;; }}
  )

;; Tern is good like, for completion
(use-package tern
  :ensure t
  :defer t
  :diminish ""
  :init
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (define-key tern-mode-keymap (kbd "C-c C-r") nil))

;; See above
(use-package company-tern
  :ensure t
  :after (company tern js2-mode)
  :defer t
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (setq-local company-backends company-backends)
                             (add-to-list 'company-backends 'company-tern))))

;; Tends to be a little crappy when finding definitions but is quite
;; useful for references. We use the default key: M-?
(when (executable-find "ag")
  (use-package xref-js2
    :ensure t
    :defer t
    :init
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))

;; Been using this one for work lately
(use-package json-reformat
  :ensure t
  :commands (json-pretty-print json-pretty-print-buffer json-reformat-region)
  :defer t)

;; js-comint. I have the need for this now
(use-package js-comint
  :ensure t
  :defer t
  :after js2-mode
  :config
  (define-key js2-mode-map (kbd "C-x C-e") 'js-send-last-sexp)
  (define-key js2-mode-map (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (define-key js2-mode-map (kbd "C-c C-s") 'js-send-buffer)
  (define-key js2-mode-map (kbd "C-c C-b") 'js-send-buffer-and-go)
  (define-key js2-mode-map (kbd "C-c C-z") 'run-js))

;; They say this is good. I have yet to give it a try
(use-package js2-refactor
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(provide 'js-programming)
;;; js-programming.el ends here
