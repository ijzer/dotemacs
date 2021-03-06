;; cl

;; [[file:nkc-mode-line.org::*cl][cl:1]]
(eval-when-compile
  (use-package cl-lib))
;; cl:1 ends here

;; use-package

;; [[file:nkc-mode-line.org::*use-package][use-package:1]]
(require 'use-package)
;; use-package:1 ends here

;; spaceline
;;    [[https://github.com/TheBB/spaceline][github]]

;; [[file:nkc-mode-line.org::*spaceline][spaceline:1]]
(use-package spaceline-config
  :config
  (when window-system
  (setq spaceline-window-numbers-unicode t)) 
  (spaceline-helm-mode 1))
;; spaceline:1 ends here

;; Use Spaceline
;;    Spaceline takes two arguments, things to display on the left-hand
;;    and right-hand sides of the mode line.

;; [[file:nkc-mode-line.org::*Use%20Spaceline][Use\ Spaceline:1]]
(spaceline-install
 `((nkc/ace-window
    :face highlight-face
    ,@(when window-system '(:tight)))
   (line-column :when active)
   buffer-position
   nkc/remote-host
   (nkc/buffer-status nkc/vc-branch)
   (nkc/buffer-id :fallback buffer-id))
 '((org-clock :when active)
   global
   major-mode
   (nkc/minor-modes :when active)))
;; Use\ Spaceline:1 ends here

;; Info+

;; [[file:nkc-mode-line.org::*Info+][Info+:1]]
(defvar nkc/info-breadcrumb-max-width 75
  "Max width for breadcrumbs in info+ mode line")

(defun nkc/parse-info-mode-line ()
  (string-match "\\`(\\(?1:[^)]+\\))\\(?2:.+\\)\\(?: > \\(?3:.+\\)\\)?\\'"
                mode-line-format)
  (let* ((filename (match-string 1 mode-line-format))
         (breadcrumbs (match-string 2 mode-line-format))
         (node (s-truncate nkc/info-breadcrumb-max-width
                           (match-string 3 mode-line-format)))
         (len (- nkc/info-breadcrumb-max-width (length node)))
         (breadcrumbs (if (< len (length breadcrumbs))
                          (concat "..." (s-right (- len 3) breadcrumbs))
                        breadcrumbs))
         (breadcrumbs (when breadcrumbs (s-split " > " breadcrumbs))))
    (setq mode-line-format `("%e"
                             (:eval (spaceline--prepare
                                     '((nkc/ace-window
                                        :face highlight-face
                                        ,@(when window-system '(:tight)))
                                       (,filename
                                        :face highlight-face)
                                       ,@breadcrumbs
                                       ,node)
                                     nil))))))

(advice-add 'Info-set-mode-line :after #'nkc/parse-info-mode-line)
;; Info+:1 ends here

;; ace window
;;     Gives the value of the window for selecting with ace-window

;; [[file:nkc-mode-line.org::*ace%20window][ace\ window:1]]
(spaceline-define-segment nkc/ace-window
  "The current window number for jumping to with ace-window.
Requires ace-window to be installed and ace-window-display mode to be
set to true."
  (let* ((win (window-parameter (selected-window) 'ace-window-path)))
    (if spaceline-window-numbers-unicode
        (spaceline--unicode-number win)
      win))
  :when (fboundp 'ace-window))
;; ace\ window:1 ends here

;; Modified buffers

;; [[file:nkc-mode-line.org::*Modified%20buffers][Modified\ buffers:1]]
(defvar nkc/buffer-status-alist
  '((read-only . "R")
    (not-modified . "")
    (modified . "~")
    (modified-outside . "~")))

(spaceline-define-segment nkc/buffer-status
  "Displays a character depending on the status of the buffer."
  (cdr (assoc
        (cond
         ((buffer-stale--default-function) 'modified-outside)
         (buffer-read-only 'read-only)
         ((buffer-modified-p) 'modified)
         (t 'not-modified))
        nkc/buffer-status-alist)))
;; Modified\ buffers:1 ends here

;; vc status

;; [[file:nkc-mode-line.org::*vc%20status][vc\ status:1]]
(defvar nkc/vc-state-char-alist
  '((up-to-date . "")
    (edited . "~")
    (needs-update . "∆")
    (needs-merge . "∇")
    (added . "+")
    (removed . "×")
    (conflict . "!")
    (missing . "?")
    (unregistered . "??")))

(spaceline-define-segment nkc/vc-state
  (let* ((backend (vc-backend buffer-file-name))
         (state (vc-state-refresh buffer-file-name backend)))
    (cdr (assoc state nkc/vc-state-char-alist))))
;; vc\ status:1 ends here

;; vc branch

;; [[file:nkc-mode-line.org::*vc%20branch][vc\ branch:1]]
(spaceline-define-segment nkc/vc-branch
  "Version control information"
  (let* ((mode vc-mode)
         (backend (vc-backend buffer-file-name)))
    (replace-regexp-in-string
     (format  "\\` \\(%s[-!:?@]\\)" (symbol-name backend)) "" mode))
  :when (and vc-mode buffer-file-name))
;; vc\ branch:1 ends here

;; Helper functions and variables

;; [[file:nkc-mode-line.org::*Helper%20functions%20and%20variables][Helper\ functions\ and\ variables:1]]
(defvar nkc/buffer-file-replacement-alist
  `((,(rx "[*Org Src" (zero-or-more not-newline) "]*"
          (optional (group "<" (zero-or-more (not (any ?>))) ">")) "]") "\\1")
    (,(rx bos "/home/" (+ (not (any ?/))) "/") "~/")
    (,user-emacs-directory "~emacs/")
    (,(rx bos "~emacs/lisp/") "~elisp/")
    (,(rx bos "~/" (zero-or-more not-newline) "doc"
          (zero-or-more not-newline) "org") "~org")
    (,(rx bos "/" (one-or-more (not (any ?/)))) ""))
  "AList in the form ((regexp . replacement)) for applying to
buffer-file-name to shorten it. Replacements are applied sequentially.")

(defvar nkc/buffer-id-max-width 40 "Max width of buffer id displayed in mode line")
(defvar nkc/buffer-id-shortener "…"
  "String inserted between prefix and directory name if buffer id is
shortened")

(defun nkc/replace-buffer-file (buffer-file)
"Replace matches on buffer-file using nkc/buffer-file-replacement-alist"
  (dolist (prefix nkc/buffer-file-replacement-alist)
    (setq buffer-file (replace-regexp-in-string (car prefix)
                                                (cadr prefix)
                                                buffer-file)))
  buffer-file)

(defun nkc/shorten-buffer-file (buffer-file max &optional connector)
  "Shorten buffer-file to max at longest by replacing directory names with
connector"
  (let* ((connector (if connector connector nkc/buffer-id-shortener))
         (filename (file-name-nondirectory buffer-file))
         (dirname (file-name-directory buffer-file))
         (prefix (car (s-match "\\`~.*?/" dirname)))
         (dirname (s-chop-prefix prefix dirname))
         (len (- max (length filename) (length prefix) (length connector))))
    (concat prefix
            (when (< len (length dirname)) connector)
            (s-right len dirname)
            filename)))

(defvar nkc/buffer-file-name nil "File name of current buffer to check for changes")
(make-variable-buffer-local 'nkc/buffer-file-name)
(defvar nkc/buffer-id nil "Shortened buffer ID to display in mode line")
(make-variable-buffer-local 'nkc/buffer-id)

(defun nkc/update-buffer-id-maybe (buffer-file)
  "Update buffer name for display if buffer-file-name has changed"
  (unless (string= buffer-file nkc/buffer-file-name)
    (setq nkc/buffer-file-name buffer-file)
    (setq nkc/buffer-id (nkc/shorten-buffer-file
                         (nkc/replace-buffer-file buffer-file)
                         nkc/buffer-id-max-width)))
  nkc/buffer-id)
;; Helper\ functions\ and\ variables:1 ends here

;; Segment definition

;; [[file:nkc-mode-line.org::*Segment%20definition][Segment\ definition:1]]
(spaceline-define-segment nkc/buffer-id
  "Current buffer ID"
  (cond
   (buffer-file-name (nkc/update-buffer-id-maybe buffer-file-name))
   ((buffer-name) (buffer-name)))
  :when (or buffer-file-name (buffer-name)))
;; Segment\ definition:1 ends here

;; Minor modes

;; [[file:nkc-mode-line.org::*Minor%20modes][Minor\ modes:1]]
(defvar nkc/minor-mode-replacer-alist '((auto-fill-function "↴")
                                        (alchemist-mode "Alc")
                                        (which-key-mode "")
                                        (company-mode "")
                                        (visual-line-mode "↲")
                                        (helm-mode "")
                                        (lispy-mode "Lispy")
                                        (org-src-mode "Src")
                                        (eldoc-mode "")
                                        (edebug-mode "∑")
                                        (visible-mode "V")
                                        (overwrite-mode "<")
                                        (isearch-mode "")
                                        (abbrev-mode "a")
                                        (doc-view-minor-mode "Doc")
                                        (image-minor-mode (:eval
                                                           (if image-type
                                                               image-type
                                                             "Img"))))
  "Alist of (MODE . LIGHTER) to replace those given in minor-mode-alist")

(defun nkc/minor-mode-replacer (mode lighter)
  (let ((replacer (cadr (assoc mode nkc/minor-mode-replacer-alist))))
    (if replacer
        replacer
      lighter)))

(spaceline-define-segment nkc/minor-modes
  "A list of minor modes. Configure the separator with 'spaceline-minor-modes-separator and the lighters with nkc/minor-mode-replacer-alist"
  (-filter
   (lambda (k) (s-present? k))
   (mapcar (lambda (mm)
             (let* ((mode (car mm))
                    (lighter (cadr mm))
                    (displayp (and (boundp mode)
                                   (symbol-value mode)))
                    (lighter (when displayp
                               (nkc/minor-mode-replacer
                                mode (s-trim (format-mode-line lighter)))))
                    (displayp (s-present? lighter)))
               (when displayp
                 lighter)))
           minor-mode-alist))
  :separator spaceline-minor-modes-separator)
;; Minor\ modes:1 ends here

;; Remote host

;; [[file:nkc-mode-line.org::*Remote%20host][Remote\ host:1]]
(spaceline-define-segment nkc/remote-host
  "Short hostname for remote buffers."
  (car (split-string (file-remote-p default-directory 'host) "\\."))
  :when (file-remote-p default-directory 'host))
;; Remote\ host:1 ends here

;; Provide

;; [[file:nkc-mode-line.org::*Provide][Provide:1]]
(provide 'nkc-mode-line)
;; Provide:1 ends here
