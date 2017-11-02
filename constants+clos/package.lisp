;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/constants+clos
  (:use
   #:cl
   #:phoe-toolbox
   #:cl-furcadia/base
   #:cl-furcadia/constants
   #:cl-furcadia/clos)
  (:export
   ;; DIGOS
   #:*digos*
   #:*wings*
   #:wing
   #:wings-name
   #:*wingable-digos*
   #:wingable-digo-p
   ))
