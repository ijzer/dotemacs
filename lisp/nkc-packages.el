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

;; info+

;; [[file:nkc-packages.org::*info+][info+:1]]
(require 'info+)
;; info+:1 ends here

;; ace window

;; [[file:nkc-packages.org::*ace%20window][ace\ window:1]]
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (add-hook 'window-configuration-change-hook 'aw-update)
  (setq aw-background nil
        aw-dispatch-always t))
;; ace\ window:1 ends here

;; Magit
;;    [[https://github.com/magit/magit][github]] [[http://magit.vc/manual/][manual]]

;; [[file:nkc-packages.org::*Magit][Magit:1]]
(use-package magit
  :bind (("C-c g s" . magit-status)
         ("C-c g p" . magit-pull)))
;; Magit:1 ends here

;; helm-config

;; [[file:nkc-packages.org::*helm-config][helm-config:1]]
(use-package helm-config
    :demand t
    :bind (("M-x" . helm-M-x)
           ("C-h a" . helm-apropos)
           ("C-x b" . helm-mini)
           ("C-x C-f" . helm-find-files)
           ("C-c h" . helm-command-prefix)
           ("C-c h o" . helm-occur)
           ("C-c h r" . helm-regexp)
           ("C-c h x" . helm-resume)
           ("C-c h y" . helm-show-kill-ring))
    :init (unbind-key "C-x c")
    :config
;; helm-list-elisp-packages
;; helm-filtered-bookmarks
;; helm-lisp-completion-at-point
;; helm-complex-command-history
;; helm-eval-expression-with-eldoc (add lispy)

    (require 'helm)
    (require 'async-bytecomp)
    (bind-keys :map helm-map
               ("<tab>" . helm-execute-persistent-action)
               ("C-i" . helm-execute-persistent-action)
               ("C-z" . helm-select-action))

    (helm-autoresize-mode 1)

    (when (executable-find "curl") (setq helm-google-suggest-use-curl-p t))

    (helm-mode 1))
;; helm-config:1 ends here

;; helm-descbinds

;; [[file:nkc-packages.org::*helm-descbinds][helm-descbinds:1]]
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds)
  :config (require 'helmconfig))
;; helm-descbinds:1 ends here

;; helm-adaptive

;; [[file:nkc-packages.org::*helm-adaptive][helm-adaptive:1]]
(use-package helm-adaptive
  :config
  (setq helm-adaptive-history-file (concat user-emacs-directory
                                           "helm/helm-adaptive-history"))
  (helm-adapative-mode 1))
;; helm-adaptive:1 ends here

;; helm-buffers

;; [[file:nkc-packages.org::*helm-buffers][helm-buffers:1]]
(use-package helm-buffers
  :config
  ((setq helm-buffers-fuzzy-matching t)))
;; helm-buffers:1 ends here

;; helm-command

;; [[file:nkc-packages.org::*helm-command][helm-command:1]]
(use-package helm-command
  :config
  (setq helm-M-x-always-save-history t
        helm-M-x-fuzzy-match t))
;; helm-command:1 ends here

;; helm-elisp

;; [[file:nkc-packages.org::*helm-elisp][helm-elisp:1]]
(use-package helm-elisp
  :config
  (setq helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t))
;; helm-elisp:1 ends here

;; Org

;; [[file:nkc-packages.org::*Org][Org:1]]
(use-package nkc-org)
;; Org:1 ends here

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
    (setq lisp-mode-initialized t)
  
    (info-lookmore-elisp-userlast)
    (info-lookmore-elisp-cl))

  (add-hook 'after-save-hook 'check-parens nil t)
  (eldoc-mode 1)
  (lispy-mode 1)
  (show-paren-mode)




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
