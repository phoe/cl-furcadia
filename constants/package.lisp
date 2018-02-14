;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/constants
  (:use
   #:cl
   #:phoe-toolbox
   #:alexandria
   #:cl-furcadia/base
   #:cl-furcadia/protocol)
  (:export
   ;; types
   #:*color-types* #:*wings* #:color #:wings #:wings-name
   ;; data files
   #:*digos* #:*gradients* #:*color-names* #:*classic-palette* #:*legacy-remaps*
   ;; variables
   #:*color-values* #:*legacy-remap-types* #:*color-code-indices* #:*genders*
   #:*wingable-digos* #:*gradient-stops* #:*gradient-stops-hair*
   #:*gradient-blends* #:*kitterspeak* #:*file-generators* #:*desc-standards*))
