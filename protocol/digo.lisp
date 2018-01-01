;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; digo.lisp

(in-package :cl-furcadia/protocol)

(define-protocol digo
    (:description "The DIGO protocol describes objects representing different ~
looks for characters in MMOSG Furcadia, commonly called digos. Each of these ~
objects gives a particular look to the furre that \"wears\" it.
\
Each digo object is uniquely identified by its index value. Some digos have ~
two indices; in this case, one of them is the main index and the other denotes ~
an alternate form of a digo."
     :tags (:cl-furcadia :digo)
     :export t)
  (:class digo () ())
  "A digo object. Each class participating in the protocol must subclass this ~
protocol class."
  (:function index ((digo digo)) unsigned-byte)
  "Returns the index number of the digo. This value should be provided when ~
instantiating the digo and is otherwise immutable."
  (:function name ((digo digo)) string)
  "Returns the name of the digo. This value should be provided when ~
instantiating the digo and is otherwise immutable."
  (:function version ((digo digo)) unsigned-byte)
  "Returns the version of the digo."
  (:function (setf version)
             ((new-value unsigned-byte) (digo digo)) unsigned-byte)
  "Sets the version of the digo."
  (:function freep ((digo digo)) boolean)
  "Returns true if the digo is free, and false otherwise."
  (:function (setf freep) ((new-value boolean) (digo digo)) boolean)
  "Sets the new free status of the digo."
  (:function exclusivep ((digo digo)) boolean)
  "Returns true if the digo is exclusive, and false otherwise."
  (:function (setf exclusivep) ((new-value boolean) (digo digo)) boolean)
  "Sets the new exclusive status of the digo."
  (:function alternate-form ((digo digo)) (or unsigned-byte null))
  "Returns the alternate form index of a digo, or NIL if the digo does not ~
have an alternate form."
  (:function (setf alternate-form)
             ((new-value (or unsigned-byte null)) (digo digo))
             (or unsigned-byte null))
  "Sets the alternate form of a digo."
  (:function wingablep ((digo digo)) boolean)
  "Returns true if the provided digo is wingable; otherwise, returns false."
  (:function (setf wingablep) ((new-value boolean) (digo digo)) boolean)
  "Sets if the provided digo is wingable."
  (:function fox-file ((digo digo)) (or null pathname))
  "Returns the pathname to the FOX file associated with the digo, or NULL if ~
there is no such file."
  (:function fox-file
             ((new-value (or null pathname)) (digo digo)) (or null pathname))
  "Sets the pathname to the FOX file associated with the digo.")
