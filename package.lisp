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
     #:cl-furcadia/news
     #:cl-furcadia/remap
     #:cl-furcadia/characters)
  (:reexport
   #:cl-furcadia/base
   #:cl-furcadia/date-parser
   #:cl-furcadia/constants
   #:cl-furcadia/clos
   #:cl-furcadia/constants+clos
   #:cl-furcadia/news
   #:cl-furcadia/remap
   #:cl-furcadia/characters))
