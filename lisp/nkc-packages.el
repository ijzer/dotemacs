;; Dependencies

;; [[file:nkc-packages.org::*Dependencies][Dependencies:1]]
(require 'use-package)
(require 'bind-key)
;; Dependencies:1 ends here

;; Magit
;;    [[https://github.com/magit/magit][github]] [[http://magit.vc/manual/][manual]]

;; [[file:nkc-packages.org::*Magit][Magit:1]]
(use-package magit
  :bind (("C-c g s" . magit-status)
         ("C-c g p" . magit-pull)))
;; Magit:1 ends here

;; Solarized
;;    [[https://github.com/sellout/emacs-color-theme-solarized][github]]

;; [[file:nkc-packages.org::*Solarized][Solarized:1]]
(load-theme 'solarized t)
;; Solarized:1 ends here

;; Provide

;; [[file:nkc-packages.org::*Provide][Provide:1]]
(provide 'nkc-packages)
;; Provide:1 ends here
