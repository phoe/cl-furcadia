;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:cl-furcadia
    (:use
     #:cl
     #:cl-furcadia/base
     #:cl-furcadia/date-parser
     #:cl-furcadia/constants
     #:cl-furcadia/clos
     #:cl-furcadia/constants+clos
     #:cl-furcadia/remap)
  (:reexport
   #:cl-furcadia/base
   #:cl-furcadia/date-parser
   #:cl-furcadia/constants
   #:cl-furcadia/clos
   #:cl-furcadia/constants+clos
   #:cl-furcadia/remap))
