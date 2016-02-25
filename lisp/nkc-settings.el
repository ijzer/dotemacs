;; GUI
;;    The mouse is the devil in emacs. Someday I'll figure out how to get
;;    rid of the damn thing entirely. It'll be beautiful, lemme tell you.

;; [[file:nkc-settings.org::*GUI][GUI:1]]
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      use-dialog-box nil)
;; GUI:1 ends here

;; Backup
;;      Source Code Pro
;;      [[http://adobe-fonts.github.io/source-code-pro/]]

;; [[file:nkc-settings.org::*Backup][Backup:1]]
(set-fontset-font "fontset-default"
                  'unicode
                  (font-spec :name "Source Code Pro" :size 13
                             :weight 'bold :width 'normal))
;; Backup:1 ends here

;; Default
;;      Meslo LG S
;;      [[https://github.com/andreberg/Meslo-Font]] 

;; [[file:nkc-settings.org::*Default][Default:1]]
(set-fontset-font "fontset-default"
                  'ascii
                  (font-spec :name "Pragmata Pro"
                             :weight 'normal :width 'normal
                             :size 9.0)
                  nil 'prepend)
;; Default:1 ends here

;; Install fonts

;; [[file:nkc-settings.org::*Install%20fonts][Install\ fonts:1]]
(set-frame-font "fontset-default")
;; Install\ fonts:1 ends here

;; Symbols

;; [[file:nkc-settings.org::*Symbols][Symbols:1]]

;; Symbols:1 ends here

;; Backups
;;    emacs likes to strew backup and autosave files everywhere. They
;;    ought to be in one place. So let's do that.

;; [[file:nkc-settings.org::*Backups][Backups:1]]
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory
                                                      "auto-save-list/") t)))
;; Backups:1 ends here


;; I habitually use version control so I don't need many backups and I
;; don't care if I don't have old ones around.

;; [[file:nkc-settings.org::*Backups][Backups:2]]
(setq delete-old-versions t
      kept-old-versions 0
      kept-new-versions 5
      version-control t
      vc-make-backup-files t
      backup-by-copying t)
;; Backups:2 ends here

;; Mode line
;;    My mode line edits got a little out of hand.

;; [[file:nkc-settings.org::*Mode%20line][Mode\ line:1]]
(use-package nkc-mode-line)
;; Mode\ line:1 ends here

;; Directory names
;;    whatbox mounts home directories in /mnt which is inaccessible to users
;;    and screws up emacs sometimes.

;; [[file:nkc-settings.org::*Directory%20names][Directory\ names:1]]
(setq directory-abbrev-alist '(("/mnt/sd[a-z][0-9]" . "/home")))
;; Directory\ names:1 ends here

;; Visual line mode

;; [[file:nkc-settings.org::*Visual%20line%20mode][Visual\ line\ mode:1]]
(visual-line-mode)
;; Visual\ line\ mode:1 ends here

;; Provide

;; [[file:nkc-settings.org::*Provide][Provide:1]]
(provide 'nkc-settings)
;; Provide:1 ends here
