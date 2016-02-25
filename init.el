;; Load path
;;    I definitely could use use-package for this, but when there's bare
;;    files in an organizational directory it just feels easier to do
;;    without it. Plus there's a ton of stuff in the lisp/ directory that
;;    i'd rather not have to specify in use-package. On the other hand
;;    it's probably better for stuff from other people to be explicitly
;;    labeled? So I might remove everything but lisp/ from this later. 

;; [[file:init.org::*Load%20path][Load\ path:1]]

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("lib" "site-lisp" "lisp")))
;; Load\ path:1 ends here

;; TODO check for cask
;;    First make sure Cask is installed and figure out where it's
;;    hidden. If it isn't, use-package will still work as long as the
;;    packages are actually installed, but we'll need to run
;;    package-initialize instead.

;; [[file:init.org::*check%20for%20cask][check\ for\ cask:1]]
(require 'cask)
  (cask-initialize)

  (require 'pallet)
  (pallet-mode t)
;  (pallet-install)
;  (pallet-update)
;; check\ for\ cask:1 ends here

;; use-package
;;    [[https://github.com/jwiegley/use-package][github]]
;;    it's a beautiful thing. cask does all the auto-load stuff through
;;    package.el but use-package can still do binding/deferral/autoload
;;    creation. On top of making it really obvious what package config
;;    stuff goes with. At some point I'll look at customize, I
;;    promise. But not yet. So use-package, obv.

;; [[file:init.org::*use-package][use-package:1]]
(eval-when-compile
  (setq use-package-verbose t)
  (require 'use-package))
;; use-package:1 ends here

;; Packages
;;    Setup for packages, package-specific bindings, that sort of thing.

;; [[file:init.org::*Packages][Packages:1]]
(use-package nkc-packages)
;; Packages:1 ends here

;; Settings
;;    Basically anything that doesn't fit into an above category. Things
;;    that would be in a use-package :config if emacs were a package.

;; [[file:init.org::*Settings][Settings:1]]
(use-package nkc-settings)
;; Settings:1 ends here
