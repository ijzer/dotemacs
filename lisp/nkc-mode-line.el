;; cl

;; [[file:nkc-mode-line.org::*cl][cl:1]]
(eval-when-compile
  (use-package cl-lib))
;; cl:1 ends here

;; use-package

;; [[file:nkc-mode-line.org::*use-package][use-package:1]]
(require 'use-package)
;; use-package:1 ends here

;; smart-mode-line
;;    [[https://github.com/Malabarba/smart-mode-line][github]]
;;    setting sml/theme to nil limits the colorizing sml adds to the
;;    modeline, which makes things a bit easier. calling sml/setup adds
;;    some useful hooks even though we're going to be rewriting a lot of
;;    the modeline variables sml sets up.

;; [[file:nkc-mode-line.org::*smart-mode-line][smart-mode-line:1]]
(use-package smart-mode-line)
;; smart-mode-line:1 ends here

;; spaceline
;;    [[https://github.com/TheBB/spaceline][github]]

;; [[file:nkc-mode-line.org::*spaceline][spaceline:1]]
(use-package spaceline-segments
  :config
  (when window-system
    (setq spaceline-window-numbers-unicode t)))
;; spaceline:1 ends here

;; Use Spaceline
;;    Spaceline takes two arguments, things to display on the left-hand
;;    and right-hand sides of the mode line.

;; [[file:nkc-mode-line.org::*Use%20Spaceline][Use\ Spaceline:1]]
(spaceline-install
 `((nkc/ace-window
    :face highlight-face
    ,@(when window-system '(:tight)))
   line-column
   ,@(if window-system '(hud :fallback buffer-position) '(buffer-position))
   ((nkc/version-control :fallback nkc/buffer-status)
    (nkc/buffer-id :fallback buffer-id))
   remote-host)
 '((org-clock :when active)
   major-mode
   (minor-modes :when active)))
;; Use\ Spaceline:1 ends here

;; Left
;;    This blocks out the spaceline segments the left side of the
;;    mode line will use. Segment definitions can be found by searching
;;    for spaceline--segment-{name}. 
;; #+NAME: spaceline-left

;; [[file:nkc-mode-line.org::*Left][spaceline-left]]
`((nkc/ace-window
   :face highlight-face
   ,@(when window-system '(:tight)))
  line-column
  ,@(if window-system '(hud :fallback buffer-position) '(buffer-position))
  ((nkc/version-control :fallback nkc/buffer-status)
   (nkc/buffer-id :fallback buffer-id))
  remote-host)
;; spaceline-left ends here

;; Right 
;;    This blocks out the spaceline segments the right side of the
;;    mode line will use. Segment definitions can be found by searching
;;    for spaceline--segment-{name}.
;; #+NAME: spaceline-right

;; [[file:nkc-mode-line.org::*Right][spaceline-right]]
'((org-clock :when active)
  major-mode
  (minor-modes :when active))
;; spaceline-right ends here

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
  :when (bound-and-true-p ace-window-display-mode))
;; ace\ window:1 ends here

;; Modified buffers

;; [[file:nkc-mode-line.org::*Modified%20buffers][Modified\ buffers:1]]
(defvar nkc/buffer-status-read-only-char "R"
  "Character to display in modeline if buffer is read-only")
(defvar nkc/buffer-status-not-modified-char ""
  "Character to display in modeline if buffer is not modified")
(defvar nkc/buffer-status-modified-char "~"
  "Character to display in modeline if buffer is modified")
(defvar nkc/buffer-status-modified-outside-char "!"
  "Character to display in modeline if buffer was modified outside emacs")

(spaceline-define-segment nkc/buffer-status
  "Displays a character depending on the status of the buffer."
  (cond
   ((buffer-stale--default-function) nkc/buffer-status-modified-outside-char)
   (buffer-read-only nkc/buffer-status-read-only-char)
   ((buffer-modified-p) nkc/buffer-status-modified-char)
   (t nkc/buffer-status-not-modified-char)))
;; Modified\ buffers:1 ends here

;; Version control
;;     vc-mode gives us useful info but takes up a ton of space.
;;     look at [[https://zavoloklom.github.io/material-design-iconic-font/cheatsheet.html][material design iconic font]] for things to display under a
;;     windowing system. assuming emacs has a way to add fonts with
;;     propertize, which i'm fairly sure it does.

;; [[file:nkc-mode-line.org::*Version%20control][Version\ control:1]]
(defvar nkc/vc-state-char-alist
  '((up-to-date . "-")
    (edited . "~")
    (needs-update . "∆")
    (needs-merge . "∇")
    (added . "+")
    (removed . "×")
    (conflict . "!")
    (missing . "?")
    (unregistered . "??")))

(spaceline-define-segment nkc/version-control
  "Version control information"
  (let ((mode vc-mode)
	(state (vc-state buffer-file-name))
	(backend (symbol-name (vc-backend buffer-file-name))))
    (concat (cdr (assoc state nkc/vc-state-char-alist))
	    (replace-regexp-in-string
	     (format  "\\` \\(%s[-!:?@]\\)" backend) "" mode)))
  :when (and vc-mode buffer-file-name))

(defun nkc/version-control ()
  "Version control information"
  (let ((mode "Git:master")
	(state 'needs-update)
	(backend "Git"))
    (concat (cdr (assoc state nkc/vc-state-char-alist))
	    (replace-regexp-in-string
	     (format  "\\`\\(%s[-!:?@]\\)" backend) "" mode))))

(nkc/version-control)
;; Version\ control:1 ends here

;; Helper functions and variables

;; [[file:nkc-mode-line.org::*Helper%20functions%20and%20variables][Helper\ functions\ and\ variables:1]]
(defvar nkc/buffer-file-replacement-alist
  `((,(rx "[*Org Src " (+ not-newline) "[ " (+ not-newline) "]*]") "")
    (,(rx "/home/" (+? not-newline) "/") "~/")
    (,user-emacs-directory "~emacs/")
    (,(rx "~emacs/lisp/") "~elisp/")
    (,(rx "~/" (+ not-newline) "doc" (+ not-newline) "org") "~org"))
  "AList in the form ((regexp . replacement)) for applying to
buffer-file-name to shorten it. Replacements are applied sequentially.")

(defvar nkc/buffer-id-max-width 40 "Max width of buffer id displayed in mode line")

(defun nkc/replace-buffer-file (buffer-file)
"Replace matches on buffer-file using nkc/buffer-file-replacement-alist"
  (dolist (prefix nkc/buffer-file-replacement-alist)
    (setq buffer-file (replace-regexp-in-string (car prefix)
						(cadr prefix)
						buffer-file)))
  buffer-file)

(defun nkc/shorten-buffer-file (buffer-file max)
  "Shorten buffer-file to (length max) by replacing directory names with '…'"
  (let* ((folders (split-string buffer-file "/"))
	 (prefix (concat (pop folders) "/")))
    (concat prefix (cl-reduce
		    (apply-partially
		     (lambda (max path segment)
		       (if (not (string-match "…" path))
			   (if (< max (+ (length path) (length segment)))
			       (concat "…/" path)
			     (concat segment "/" path))
			 path))
		     (- max (length prefix) 1))
		    (reverse folders)))))

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
  (nkc/update-buffer-id-maybe buffer-file-name)
  :when buffer-file-name)
;; Segment\ definition:1 ends here

;; Provide

;; [[file:nkc-mode-line.org::*Provide][Provide:1]]
(provide 'nkc-mode-line)
;; Provide:1 ends here
