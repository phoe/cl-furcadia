;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/constants
  (:use
   #:cl
   #:phoe-toolbox
   #:cl-furcadia/base
   #:cl-furcadia/protocol)
  (:export
   #:color #:wings #:wings-name
   #:*color-types*  #:*color-names* #:*color-values* #:*color-code-indices*
   #:*gradients* #:*classic-palette* #:*kitterspeak*
   #:*digos* #:*wings* #:*wingable-digos* #:*genders*))
