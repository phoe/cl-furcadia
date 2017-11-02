;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/base
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox)
  (:export
   ;; utility functions
   #:from-220
   #:to-220
   #:name-shortname
   #:read-data-file))
