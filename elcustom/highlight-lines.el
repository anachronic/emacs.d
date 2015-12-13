(eval-and-compile
  (if (boundp 'xemacs-logo)
      (if (not (load "overlay" 'noerr))
	  (error "\
highlight-current-line.el: ** This package requires overlays.  Abort"))))

;; Compatibility code - blob for those without the custom library:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; can be set by user

(defgroup highlight-current-line nil
  "Highlight line where the cursor is."
  :load 'highlight-current-line
  :group 'faces) ;; or 'matching??

(defcustom highlight-current-line-ignore-regexp
  (concat
   "Faces\\|Colors\\| \\*Mini"
   ;; for example:
   ;; "\\|RMAIL.*summary\\|\\*Group\\|\\*Summary"
   )
  "*Regexps for buffers to ignore.
Used by `highlight-current-line-ignore-function'."
  :type  'regexp
  :group 'highlight-current-line)

(defcustom highlight-current-line-whole-line t
  "*If non-nil, mark up to `end-of-line'.  If nil, mark up to window-border.
Use `highlight-current-line-whole-line-on' to set this value."
  :type  'boolean
  :group 'highlight-current-line)

(defcustom highlight-current-line-high-faces '()
  "*Lines containing one of this faces are not highlighted."
  :type  'list
  :group 'highlight-current-line)

(defface highlight-current-line-face
  '((t (:background "wheat")))
    "Face used to highlight current line."
  :group 'highlight-current-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; should not be set by user

(defconst highlight-current-line-version "0.57"
  "Version number." )

(defvar highlight-current-line-minor-mode nil
  "Non-nil if using highlight-current-line mode as a minor mode.
Use the command `highlight-current-line-minor-mode' to toggle or set this
variable.")
(make-variable-buffer-local 'highlight-current-line-minor-mode)

(defvar highlight-current-line-overlay
  ;; Dummy initialization
  (make-overlay 1 1)
  "Overlay for highlighting.")

;; Set face-property of overlay
(overlay-put highlight-current-line-overlay
	     'face 'highlight-current-line-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Internal function for test
(defun highlight-current-line-reload ()
  "Reload library highlight-current-line for test purposes."
  (unload-feature 'highlight-current-line)
  (load-library "highlight-current-line"))

;; Decide whether to highlight the buffer.
(defun highlight-current-line-ignore-function  ()
  "Check current buffer name against `highlight-current-line-ignore-regexp'.
Inhibits global enabling of highlight-current-line on buffer whose name
match this regexp."
  (if (or (equal "" highlight-current-line-ignore-regexp)
	  (not highlight-current-line-ignore-regexp))
      nil
    (string-match highlight-current-line-ignore-regexp (buffer-name))))

(defvar highlight-current-line-globally)

;; Post-Command-Hook for highlighting
(defun highlight-current-line-hook ()
  "Post-Command-Hook for highlighting."
  (condition-case ()
      (if (or highlight-current-line-minor-mode
              (and highlight-current-line-globally
                   (or (not (fboundp 'highlight-current-line-ignore-function))
                       (not (highlight-current-line-ignore-function)))))
          (let ((current-point (point)))

            ;; Set overlay
            (let ((beg (progn (beginning-of-line) (point)))
                  (end (progn (if highlight-current-line-whole-line
                                  (forward-line 1)
                                (end-of-line))
                              (point))))
              (if (delete nil (mapcar
                                 (lambda( face )
                                   (text-property-any beg end 'face face))
                                 highlight-current-line-high-faces))
                  (delete-overlay highlight-current-line-overlay)
                (move-overlay highlight-current-line-overlay
                              beg end (current-buffer)))

              (goto-char current-point))))
    (error nil)))

(defconst highlight-current-line-no-color (if (boundp 'xemacs-logo)
                                              '[]
                                            nil)
  "'color' value that represents \"no color\".")

;; Compatibility code
(defun highlight-current-line-on (&optional on-off)
  "Switch highlighting of cursor-line on/off globally.
Key: \\[highlight-current-line-on]"
  (interactive (list (y-or-n-p "Highlight line with cursor? ")))
  (setq-default highlight-current-line-globally on-off)
  (highlight-current-line on-off nil))

;; Compatibility code - Set foregroundcolor of cursor-line.
(defun highlight-current-line-set-fg-color (color)
  "Set foregroundcolor for highlighting cursor-line to COLOR.
Key: \\[highlight-current-line-set-fg-color]"
  (interactive "sForeground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-foreground 'highlight-current-line-face color))

;; Compatibility code - Set backgroundcolor of cursor-line.
(defun highlight-current-line-set-bg-color (color)
  "Set backgroundcolor for highlighting cursor-line to COLOR.
Key: \\[highlight-current-line-set-bg-color]"
  (interactive "sBackground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-background 'highlight-current-line-face color))

;; Compatibility code - Enable/Disable whole line marking
(defun highlight-current-line-whole-line-on (&optional on-off)
  "Switch highlighting of whole line ON-OFF.
Key: \\[highlight-current-line-whole-line-on]"
  (interactive (list (y-or-n-p "Highlight whole line? ")))
  (setq highlight-current-line-whole-line on-off))

;; Enable/Disable Highlighting
(defun highlight-current-line (&optional on-off local)
  "Switch highlighting of cursor-line ON-OFF
If LOCAL is non-nil, do so locally for the current buffer only."
  (cond
   (on-off
    (if (or (= emacs-major-version 20)
            (string-match "XEmacs" emacs-version))
        (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook 'highlight-current-line-hook nil local)
    (if (boundp 'server-switch-hook)
        (add-hook 'server-switch-hook 'highlight-current-line-hook nil local))
    (if (boundp 'gnuserv-visit-hook)
        (add-hook 'gnuserv-visit-hook 'highlight-current-line-hook nil local)))
   (t
    (if  (boundp 'server-switch-hook)
        (remove-hook 'server-switch-hook 'highlight-current-line-hook local))
    (if (boundp 'gnuserv-visit-hook)
        (remove-hook 'gnuserv-visit-hook 'highlight-current-line-hook local))
    (remove-hook 'post-command-hook 'highlight-current-line-hook t)
    (delete-overlay highlight-current-line-overlay))))

;;;###autoload
(defun highlight-current-line-minor-mode (&optional arg)
  "Toggle highlight-current-line minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise.
You can customize the face of the highlighted line and whether the entire
line is hightlighted by customizing the group highlight-current-line."
  (interactive "P")
  (setq highlight-current-line-minor-mode
        (if (null arg)
            (not highlight-current-line-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if highlight-current-line-minor-mode
      (highlight-current-line t t)
    (highlight-current-line nil t)))

(or (assq 'highlight-current-line-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (append minor-mode-alist
                  (list '(highlight-current-line-minor-mode " hcl")))))

(defcustom highlight-current-line-globally nil
  "*Whether to enable `highlight-current-line-minor-mode' automatically.
This affects only files visited after this variable is set.
Buffers will not be enabled if they match the regular expression in
`highlight-current-line-ignore-regexp'."
  :type  'boolean
  :require 'highlight-current-line
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (highlight-current-line t nil)
           (highlight-current-line nil nil)))
  :group 'highlight-current-line)

(highlight-current-line-set-bg-color "#222222")
(highlight-current-line-on t)

(provide 'highlight-lines)
