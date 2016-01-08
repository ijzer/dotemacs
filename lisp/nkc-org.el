;; Packages

;; [[file:nkc-org.org::*Packages][Packages:1]]
(require 'use-package)

(use-package org)
;; Packages:1 ends here

;; Languages

;; [[file:nkc-org.org::*Languages][Languages:1]]
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (ledger . t)))
;; Languages:1 ends here

;; Org Src minor mode

;; [[file:nkc-org.org::*Org%20Src%20minor%20mode][Org\ Src\ minor\ mode:1]]
(setq org-src-window-setup 'current-window
      org-edit-src-auto-save-idle-delay 30)
;; Org\ Src\ minor\ mode:1 ends here

;; Default header args
;;    The important ones here are starred and are changed from the
;;    defaults. The rest are p much just there so I can see them all in
;;    one place. 

;; [[file:nkc-org.org::*Default%20header%20args][Default\ header\ args:1]]
(setq org-babel-default-header-args '((:session . "none")
                                     (:results . "value drawer silent")
                                     (:exports . "code")
                                     (:cache . "no")
                                     (:noweb . "no-export") ;***
                                     (:hlines . "no")
                                     (:tangle . "yes") ;***
                                     (:comments . "both"))) ;***
;; Default\ header\ args:1 ends here

;; Tangle certain org files on save

;; [[file:nkc-org.org::*Tangle%20certain%20org%20files%20on%20save][Tangle\ certain\ org\ files\ on\ save:1]]
(defsubst funcs-into-hook (hook &rest func-list)
  (dolist (func func-list) (add-hook hook func)))

(add-hook 'after-save-hook (lambda () (when (string-match
                                        (rx
                                         bos
                                         (+ not-newline)
                                         "/.emacs.d/"
                                         (+ not-newline)
                                         ".org")
                                        buffer-file-name)
                                   (org-babel-tangle))))
;; Tangle\ certain\ org\ files\ on\ save:1 ends here

;; Use eldoc with org files

;; [[file:nkc-org.org::*Use%20eldoc%20with%20org%20files][Use\ eldoc\ with\ org\ files:1]]
(add-hook 'org-mode-hook #'org-eldoc-load)
(add-hook 'org-mode-hook #'eldoc-mode)
;; Use\ eldoc\ with\ org\ files:1 ends here

;; Don't ask for confirmation on evaluates

;; [[file:nkc-org.org::*Don't%20ask%20for%20confirmation%20on%20evaluates][Don\'t\ ask\ for\ confirmation\ on\ evaluates:1]]
(setq org-confirm-babel-evaluate (lambda (lang body)
                                   (cond
                                    ((string= lang "emacs-lisp") nil)
                                    (t t))))
;; Don\'t\ ask\ for\ confirmation\ on\ evaluates:1 ends here

;; Provide

;; [[file:nkc-org.org::*Provide][Provide:1]]
(provide 'nkc-org)
;; Provide:1 ends here
