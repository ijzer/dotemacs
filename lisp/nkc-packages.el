;; Dependencies

;; [[file:nkc-packages.org::*Dependencies][Dependencies:1]]
(require 'use-package)
(require 'bind-key)
;; Dependencies:1 ends here

;; hook-into-modes
;;     stolen from [[https://github.com/jwiegley/dot-emacs][jww's dot emacs]]. adds a function to each of a list of
;;     modes. needs to be used with apply.

;; [[file:nkc-packages.org::*hook-into-modes][hook-into-modes:1]]
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))
;; hook-into-modes:1 ends here

;; Magit
;;    [[https://github.com/magit/magit][github]] [[http://magit.vc/manual/][manual]]

;; [[file:nkc-packages.org::*Magit][Magit:1]]
(use-package magit
  :bind (("C-c g s" . magit-status)
         ("C-c g p" . magit-pull)))
;; Magit:1 ends here

;; Org

;; [[file:nkc-packages.org::*Org][Org:1]]
(use-package nkc-org)
;; Org:1 ends here

;; Generic

;; [[file:nkc-packages.org::*Generic][Generic:1]]
(setq-default fill-column 75
	      auto-fill-function 'do-auto-fill)
;; Generic:1 ends here

;; Lispy
;;      [[https://github.com/abo-abo/lispy][github]]

;; [[file:nkc-packages.org::*Lispy][Lispy:1]]
(use-package lispy
  :defer t)
;; Lispy:1 ends here

;; Lisp editing modes
;;      Sets up a hook to turn on various good things when using a lisp
;;      mode. Add additional modes to lisp-modes if needed.

;; [[file:nkc-packages.org::*Lisp%20editing%20modes][Lisp\ editing\ modes:1]]
(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode))
(defvar lisp-mode-hooks
  (mapcar (lambda (mode)
            (intern
             (concat (symbol-name mode) "-hook")))
          lisp-modes))

(defvar lisp-mode-initialized nil)

(defun nkc/lisp-mode-hook ()
  (unless lisp-mode-initialized
    (setq lisp-mode-initialized t))

  (add-hook 'after-save-hook 'check-parens nil t)
  (eldoc-mode)
  (lispy-mode 1)

  (font-lock-add-keywords
   nil
   `((,(rx "(" symbol-start (group "lambda") symbol-end " (")
      (0 (ignore
          (compose-region (match-beginning 1)
                          (match-end 1) ?λ)))))))

(apply #'hook-into-modes 'nkc/lisp-mode-hook lisp-mode-hooks)
;; Lisp\ editing\ modes:1 ends here

;; Minibuffer evals

;; [[file:nkc-packages.org::*Minibuffer%20evals][Minibuffer\ evals:1]]

;; Minibuffer\ evals:1 ends here

;; Solarized
;;    [[https://github.com/sellout/emacs-color-theme-solarized][github]]

;; [[file:nkc-packages.org::*Solarized][Solarized:1]]
(load-theme 'solarized t)
;; Solarized:1 ends here

;; Provide

;; [[file:nkc-packages.org::*Provide][Provide:1]]
(provide 'nkc-packages)
;; Provide:1 ends here
