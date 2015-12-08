
;; UI Changes
;;    The mouse is the devil in emacs. Someday I'll figure out how to get
;;    rid of the damn thing entirely. It'll be beautiful, lemme tell you.

;; [[file:~/.emacs.d/lisp/nkc-settings.org::*UI%20Changes][UI\ Changes:1]]

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      use-dialog-box nil)

;; UI\ Changes:1 ends here

;; Backups
;;    emacs likes to strew backup and autosave files everywhere. They
;;    ought to be in one place. So let's do that.

;; [[file:~/.emacs.d/lisp/nkc-settings.org::*Backups][Backups:1]]

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory
                                                      "auto-save-list") t)))

;; Backups:1 ends here

;; I habitually use version control so I don't need many backups and I
;;    don't care if I don't have old ones around.

;; [[file:~/.emacs.d/lisp/nkc-settings.org::*Backups][Backups:1]]

(setq delete-old-versions t
      kept-old-versions 0
      kept-new-versions 5
      version-control t
      vc-make-backup-files t
      backup-by-copying t)

;; Backups:1 ends here

;; Provide

;; [[file:~/.emacs.d/lisp/nkc-settings.org::*Provide][Provide:1]]

(provide 'nkc-settings)

;; Provide:1 ends here
