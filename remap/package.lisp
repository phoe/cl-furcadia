;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/remap
  (:use
   #:cl
   #:alexandria
   #:cl-furcadia/base
   #:cl-furcadia/constants)
  (:export
   ;; functions
   #:color-code-gradient
   #:all-gradients
   #:remap))
