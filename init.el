
;; Load path
;;    I definitely could use use-package for this, but when there's bare
;;    files in an organizational directory it just feels easier to do
;;    without it. Plus there's a ton of stuff in the lisp/ directory that
;;    i'd rather not have to specify in use-package. On the other hand
;;    it's probably better for stuff from other people to be explicitly
;;    labeled? So I might remove everything but site-lisp/use-package/
;;    and lisp/ from this later.

;; [[file:~/.emacs.d/init.org::*Load%20path][Load\ path:1]]

(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("lib" "site-lisp" "lisp" "site-lisp/use-package")))

;; Load\ path:1 ends here

;; use-package
;;    [[https://github.com/jwiegley/use-package][github]]
;;    it's a beautiful thing. cask does all the auto-load stuff through
;;    package.el but use-package can still do binding/deferral/autoload
;;    creation. On top of making it really obvious what package config
;;    stuff goes with. At some point I'll look at customize, I
;;    promise. But not yet. So use-package, obv.

;; [[file:~/.emacs.d/init.org::*use-package][use-package:1]]

(eval-when-compile
  (setq use-package-verbose t)
  (require 'use-package))

;; use-package:1 ends here

;; TODO check for cask
;;    First make sure Cask is installed and figure out where it's
;;    hidden. If it isn't, use-package will still work as long as the
;;    packages are actually installed, but we'll need to run
;;    package-initialize instead.

;; [[file:~/.emacs.d/init.org::*check%20for%20cask][check\ for\ cask:1]]

(use-package cask
  :load-path "~/.cask/bin"
  :config
  (cask-initialize)

  (use-package pallet
    :config
    (pallet-mode t)
    (pallet-install)
    (pallet-update)))

;; check\ for\ cask:1 ends here

;; Settings
;;    Basically anything that doesn't fit into an above category. Things
;;    that would be in a use-package :config if emacs were a package.

;; [[file:~/.emacs.d/init.org::*Settings][Settings:1]]

(use-package nkc-settings)

;; Settings:1 ends here
