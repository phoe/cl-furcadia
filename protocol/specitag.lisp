;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; specitag.lisp

(in-package :cl-furcadia/protocol)

(define-protocol specitag
    (:documentation "The SPECITAG protocol describes objects representing ~
per-character images for characters in MMOSG Furcadia, commonly called ~
specitags. Each of these objects is shown in front of each speech line made by ~
the furre, giving additional customization.
\
Each specitag object is uniquely identified by its index value and may or may ~
not be remappable. Each custom specitag is assigned to a furre."
     :tags (:cl-furcadia :specitag)
     :export t)
  (:class specitag () ())
  "A specitag object. Each class participating in the protocol must subclass ~
this protocol class."
  (:function sid ((specitag specitag)) unsigned-byte)
  "Returns the index number of the specitag. This value should be provided ~
when instantiating the specitag and is otherwise immutable."
  (:function image-data ((specitag specitag)) vector)
  "Returns the image data of the specitag."
  (:function (setf image-data) ((new-value vector) (specitag specitag)) vector)
  "Sets the image data of the specitag."
  (:function remappedp ((specitag specitag)) boolean)
  "Returns true if the specitag is remapped, and false otherwise."
  (:function (setf remappedp) ((new-value boolean) (specitag specitag)) boolean)
  "Sets the new remapped status of the specitag."
  (:function furre ((specitag specitag)) furre)
  "Returns the furre associated with the specitag."
  (:function (setf furre) ((new-value furre) (specitag specitag)) furre)
  "Sets the furre associated with the specitag.")

(execute-protocol specitag)
