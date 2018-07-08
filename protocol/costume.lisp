;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; costume.lisp

(in-package :cl-furcadia/protocol)

(define-protocol costume
    (:documentation "The COSTUME protocol describes objects representing ~
costumes in MMOSG Furcadia. Each costume contains complete information ~
required to describe an individual furre's appearance, including its textual ~
and visual parts as well as metainformation, such as AFK times.
\
A furre may have multiple costumes but a costume belongs to a single furre. ~
Despite that fact, costumes are loosely coupled with characters.
\
Indirect instances of this class should be returned every time client code ~
successfully accesses Furcadia WS to retrieve costumes from the servers."
     :tags (:cl-furcadia :costume)
     :export t)
  (:class costume () ())
  "A costume object. Each class participating in this protocol must subclass ~
this protocol class."
  (:function cid ((costume costume)) unsigned-byte)
  "Returns the costume's CID. This value should be provided when instantiating ~
the costume and is otherwise immutable."
  (:function furre ((costume costume)) (or furre null))
  "Returns the furre this costume belongs to, or NIL if the costume is not ~
associated with a furre."
  (:function (setf furre) ((new-value (or furre null)) (costume costume))
             (or furre null))
  "Sets the furre this costume belongs to."
  (:function order ((costume costume)) unsigned-byte)
  "Returns the numerical order of the costume on the costume list of a furre."
  (:function (setf order) ((new-value unsigned-byte) (costume costume))
             unsigned-byte)
  "Sets the numerical order of the costume on the costume list of a furre."
  )
