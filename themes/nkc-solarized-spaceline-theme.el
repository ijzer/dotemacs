;; Init

;; [[file:nkc-solarized-spaceline-theme.org::*Init][Init:1]]
(require 'solarized-definitions)
(require 'nkc-solarized)
;; Init:1 ends here

;; Create theme

;; [[file:nkc-solarized-spaceline-theme.org::*Create%20theme][Create\ theme:1]]
(create-solarized-theme nkc-solarized-spaceline
                        "Solarized theme for Spaceline"
                        (nkc/create-solarized-facespec
                         '(
                           (nkc/spaceline-line-active (fg-base2 bg-base02 fmt-revbb))
                           (nkc/spaceline-face1-active (fg-base1 (:inherit spaceline-line-active)))
                           (nkc/spaceline-face2-active (fg-base3 (:inherit spaceline-line-active)))
                           (nkc/spaceline-line-inactive (fg-base0 bg-base02 fmt-revbb))
                           (nkc/spaceline-face1-inactive (fg-base1 (:inherit spaceline-line-active)))
                           (nkc/spaceline-face2-inactive (fg-base00 (:inherit spaceline-line-active)))
                           (nkc/spaceline-highlight-active (bg-orange (:inherit
                                                                       spaceline-line-active)))
                           (nkc/spaceline-highlight-inactive (bg-orange (:inherit
                                                                         spaceline-line-inactive))))))
;; Create\ theme:1 ends here

;; Facespec definitions
;; #+NAME: facespec-definition

;; [[file:nkc-solarized-spaceline-theme.org::*Facespec%20definitions][facespec-definition]]
'(
  (nkc/spaceline-line-active (fg-base2 bg-base02 fmt-revbb))
  (nkc/spaceline-face1-active (fg-base1 (:inherit spaceline-line-active)))
  (nkc/spaceline-face2-active (fg-base3 (:inherit spaceline-line-active)))
  (nkc/spaceline-line-inactive (fg-base0 bg-base02 fmt-revbb))
  (nkc/spaceline-face1-inactive (fg-base1 (:inherit spaceline-line-active)))
  (nkc/spaceline-face2-inactive (fg-base00 (:inherit spaceline-line-active)))
  (nkc/spaceline-highlight-active (bg-orange (:inherit
                                              spaceline-line-active)))
  (nkc/spaceline-highlight-inactive (bg-orange (:inherit
                                                spaceline-line-inactive))))
;; facespec-definition ends here
