;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/constants
  (:use
   #:cl
   #:phoe-toolbox
   #:cl-furcadia
   #:cl-furcadia/clos)
  (:export
   ;; REMAP
   #:color
   #:*color-types*
   #:*gradients*
   #:*color-names*
   #:*color-values*
   #:*color-code-indices*
   #:*genders*
   ;; DIGOS
   #:*digos*
   #:*wings*
   #:wing
   #:wings-name
   #:*wingable-digos*
   #:wingable-digo-p
   ))
