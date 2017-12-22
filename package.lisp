;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:cl-furcadia
  (:use
   #:cl
   #:cl-furcadia/base
   #:cl-furcadia/protocol
   #:cl-furcadia/clos
   #:cl-furcadia/constants
   #:cl-furcadia/date-parser
   #:cl-furcadia/news
   #:cl-furcadia/remap
   #:cl-furcadia/ws
   )
  (:reexport
   #:cl-furcadia/base
   #:cl-furcadia/protocol
   #:cl-furcadia/clos
   #:cl-furcadia/constants
   #:cl-furcadia/date-parser
   #:cl-furcadia/news
   #:cl-furcadia/remap
   #:cl-furcadia/ws
   ))
