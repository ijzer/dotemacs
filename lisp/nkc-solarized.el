;; Dependencies

;; [[file:nkc-solarized.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'solarized-definitions)
;; Dependencies:1 ends here

;; Expand Attribute Abbreviations
;;    (nkc/expand-solarized-attributes (attributes))
;;    attribute is of the form {fg/bg}-color or {fmt}-attribute
;;    '(fg-color1 bg-color2 fmt-attr ...) => (:foreground color1 
;;                                           :background color2
;;                                           :attr {t/f/'unspecified}
;;                                           ...)

;; [[file:nkc-solarized.org::*Expand%20Attribute%20Abbreviations][Expand\ Attribute\ Abbreviations:1]]
(defun nkc/expand-solarized-attributes (attributes)
  (let* ((bold (if solarized-bold 'bold 'unspecified))
         (bright-bold (if solarized-bold 'unspecified 'bold))
         (underline (if solarized-underline t 'unspecified))
         (opt-under 'unspecified)
         (italic (if solarized-italic 'italic 'unspecified))

         (solarized-translate-list 
          `((bg-back :background back)
            (bg-base03 :background base03)
            (bg-base02 :background base02)
            (bg-base01 :background base01)
            (bg-base00 :background base00)
            (bg-base0 :background base0)
            (bg-base1 :background base1)
            (bg-base2 :background base2)
            (bg-base3 :background base3)
            (bg-green :background green)
            (bg-yellow :background yellow)
            (bg-orange :background orange)
            (bg-red :background red)
            (bg-magenta :background magenta)
            (bg-violet :background violet)
            (bg-blue :background blue)
            (bg-cyan :background cyan)
          
            (fg-base03 :foreground base03)
            (fg-base02 :foreground base02)
            (fg-base01 :foreground base01)
            (fg-base00 :foreground base00)
            (fg-base0 :foreground base0)
            (fg-base1 :foreground base1)
            (fg-base2 :foreground base2)
            (fg-base3 :foreground base3)
            (fg-green :foreground green)
            (fg-yellow :foreground yellow)
            (fg-orange :foreground orange)
            (fg-red :foreground red)
            (fg-magenta :foreground magenta)
            (fg-violet :foreground violet)
            (fg-blue :foreground blue)
            (fg-cyan :foreground cyan)
          
            (fmt-none nil)
            (fmt-bold :weight ,bold)
            (fmt-bldi :weight ,bold :slant ,italic)
            (fmt-undr :underline ,underline)
            (fmt-undb :weight ,bold :underline ,underline)
            (fmt-undi :slant ,italic :underline ,underline)
            (fmt-uopt :underline ,opt-under)
          
            (fmt-curl-red :underline (:color "#dc322f" :style wave))
            (fmt-curl-yellow :underline (:color "#b58900" :style wave))
            (fmt-ital :slant ,italic)
          
            (fmt-stnd :inverse-video t)
            (fmt-revr :inverse-video t)
            (fmt-revb :weight ,bold :inverse-video t)
            (fmt-revbb :weight ,bright-bold :inverse-video t)
            (fmt-revbbu :weight ,bright-bold :underline ,underline :inverse-video t))))

    (cl-reduce (lambda (c l)
                 (if (atom c)
                     (cons c l)
                   (append c l)))
               (cl-sublis solarized-translate-list attributes)
               :from-end t)))
;; Expand\ Attribute\ Abbreviations:1 ends here

;; Build Facespec List
;;    create a list of facespecs using solarized's create-face-spec for
;;    passing to custom-theme-set-faces. basic definitions can be of the
;;    form described in the elisp manual under Display/Faces/Face
;;    Attributes or using nkc/expand-solarized-attributes

;; [[file:nkc-solarized.org::*Build%20Facespec%20List][Build\ Facespec\ List:1]]
(defun nkc/create-solarized-facespec (faces)
  (mapcar (lambda (facespec)
            (let ((face (car facespec))
                  (attrs (cadr facespec)))
              (create-face-spec face
                                (nkc/expand-solarized-attributes attrs))))
          faces))
;; Build\ Facespec\ List:1 ends here

;; Provide

;; [[file:nkc-solarized.org::*Provide][Provide:1]]
(provide 'nkc-solarized)
;; Provide:1 ends here
