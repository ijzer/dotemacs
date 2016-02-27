;; GUI
;;    The mouse is the devil in emacs. Someday I'll figure out how to get
;;    rid of the damn thing entirely. It'll be beautiful, lemme tell you.

;; [[file:nkc-settings.org::*GUI][GUI:1]]
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t
      use-dialog-box nil)
(if (version< emacs-version "25")
    (scroll-bar-mode -1)
  (menu-bar-no-scroll-bar))
;; GUI:1 ends here

;; Font
;;    Source Code Pro
;;    [[http://adobe-fonts.github.io/source-code-pro/]]
;;    Meslo LG S
;;    [[https://github.com/andreberg/Meslo-Font]] 

;; [[file:nkc-settings.org::*Font][Font:1]]
(if (version< emacs-version "25")
    (progn
      (set-fontset-font "fontset-default"
                        'ascii
                        (font-spec :name "Fira Code"
                                   :weight 'normal :width 'normal
                                   :size 9.0)
                        nil 'prepend)
    (set-frame-font "fontset-default")))
;; Font:1 ends here

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

;; Backward Kill Word

;; [[file:nkc-settings.org::*Backward%20Kill%20Word][Backward\ Kill\ Word:1]]
(bind-key "C-w" 'backward-kill-word)
;; Backward\ Kill\ Word:1 ends here

;; Provide

;; [[file:nkc-settings.org::*Provide][Provide:1]]
(provide 'nkc-settings)
;; Provide:1 ends here
