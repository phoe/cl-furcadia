;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox)
  (:export
   ;; utility functions
   #:from-220
   #:to-220))

(defpackage #:cl-furcadia/remap
  (:use
   #:cl
   #:cl-furcadia)
  (:export
   ;; types
   #:color
   ;; variables
   #:*color-types*
   #:*gradients*
   #:*color-names*
   #:*color-values*
   #:*color-code-indices*
   #:*genders*))
