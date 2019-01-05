;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; portrait.lisp

(in-package :cl-furcadia/protocol)

(define-protocol portrait
    (:documentation "The PORTRAIT protocol describes objects representing ~
graphical portraits in the MMOSG Furcadia. Each portrait is a 95x95 graphic ~
that may come in one of three formats (8-bit, 24-bit, FOX) and may or may not ~
be remappable.
\
Each portrait belongs to a furre."
     :tags (:cl-furcadia :portrait)
     :export t)
  (:class portrait () ())
  "A portrait object. Each class participating in this protocol must subclass ~
this protocol class."
  (:function pid ((portrait portrait)) integer)
  "Returns the ID of the portrait."
  (:function (setf pid) ((new-value integer) (portrait portrait)) integer)
  "Sets the ID of the portrait."
  (:function furre ((portrait portrait)) furre)
  "Returns the furre the portrait belongs to."
  (:function (setf furre) ((new-value furre) (portrait portrait)) furre)
  "Sets the furre the portrait belongs to."
  (:function portrait-type ((portrait portrait)) (member :8-bit :24-bit :fox))
  "Returns the portrait's type."
  (:function (setf portrait-type)
             ((new-value (member :8-bit :24-bit :fox)) (portrait portrait))
             (member :8-bit :24-bit :fox))
  "Sets the portrait's type."
  (:function remappedp ((portrait portrait)) boolean)
  "Returns whether the portrait is remappable."
  (:function (setf remappedp) ((new-value boolean) (portrait portrait)) boolean)
  "Sets whether the portrait is remappable."
  (:function data ((portrait portrait)) vector)
  "Returns the portrait's data."
  (:function (setf data) ((new-value vector) (portrait portrait)) vector)
  "Sets the portrait's data.")

(execute-protocol portrait)
