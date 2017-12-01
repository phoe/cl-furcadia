;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/constants
  (:use
   #:cl
   #:phoe-toolbox
   #:cl-furcadia/base)
  (:export
   ;; REMAP
   #:color
   #:*color-types*
   #:*gradients*
   #:*color-names*
   #:*color-values*
   #:*color-code-indices*
   #:*genders*))