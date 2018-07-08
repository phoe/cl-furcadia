;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; named.lisp

(in-package :cl-furcadia/protocol)

(define-protocol named
    (:documentation "The NAMED protocol describes objects which have a name - ~
a human-readable string that denotes that object's identity, along with other ~
attributes of that object."
     :tags (:cl-furcadia :named)
     :export t)
  (:class named () ())
  "A named object. See protocol NAMED for details."
  (:function name ((object named)) string)
  "Returns the name of the named object."
  (:function (setf name) ((new-value string) (object named)) string)
  "Sets the name of the named object.")

(execute-protocol named)
