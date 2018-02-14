;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/remap
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:cl-furcadia/base
   #:cl-furcadia/constants)
  (:export
   ;; functions
   #:color-code-gradient
   #:all-gradients
   #:remap #:remap-all
   #:8bit-32bit #:24bit-32bit))
