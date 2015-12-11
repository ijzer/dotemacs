;; Packages

;; [[file:nkc-org.org::*Packages][Packages:1]]
(require 'use-package)

(use-package org)
;; Packages:1 ends here

;; Open source code editing buffer in same window

;; [[file:nkc-org.org::*Open%20source%20code%20editing%20buffer%20in%20same%20window][Open\ source\ code\ editing\ buffer\ in\ same\ window:1]]
(setq org-src-window-setup 'current-window)
;; Open\ source\ code\ editing\ buffer\ in\ same\ window:1 ends here

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

;; Provide

;; [[file:nkc-org.org::*Provide][Provide:1]]
(provide 'nkc-org)
;; Provide:1 ends here
