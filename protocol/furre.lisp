;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; furre.lisp

(in-package :cl-furcadia/protocol)

(define-protocol furre
    (:documentation "The FURRE protocol describes objects representing ~
playable characters in MMOSG Furcadia; they are called furres. These ~
characters are able to be logged into the game, post messages for other ~
characters to see and their players to read, change their appearances, etc..
\
Each furre belongs to an account.
\
Indirect instances of this class should be returned every time client code ~
successfully accesses Furcadia WS to retrieve characters from the servers.
\
Each furre downloaded from the Furcadia WS has a so-called \"Last Appearance\" ~
costume embedded in its data. This is represented by the fact that the ~
protocol class FURRE is a subclass of protocol class COSTUME."
     :tags (:cl-furcadia :furre)
     :dependencies (named costume)
     :export t)
  (:class furre (named) ())
  "A furre object. Each class participating in this protocol must subclass ~
this protocol class."
  (:function uid ((furre furre)) unsigned-byte)
  "Returns the furre's UID. This value should be provided when instantiating ~
the furre and is otherwise immutable."
  (:function shortname ((furre furre)) string)
  "Returns the furre's shortname, computed from the furre's name."
  (:function last-login ((furre furre)) unsigned-byte)
  "Returns the date when the furre was last logged in."
  (:function (setf last-login)
             ((new-value unsigned-byte) (furre furre)) unsigned-byte)
  "Sets the date when the furre was last logged in."
  (:function digos ((furre furre)) list)
  "Returns the list of digos owned by the furre." ;; TODO return DIGOs
  (:function (setf digos) ((new-value list) (furre furre)) list)
  "Sets the list of digos owned by the furre." ;; TODO return DIGOs
  (:function lifers ((furre furre)) list)
  "Returns the list of digos owned by the furre for life." ;; TODO return DIGOs
  (:function (setf lifers) ((new-value list) (furre furre)) list)
  "Sets the list of digos owned by the furre for life." ;; TODO return DIGOs
  (:function portraits ((furre furre)) list)
  "Returns the list of portraits owned by the furre." ;; TODO portrait protocol
  (:function (setf portraits) ((new-value list) (furre furre)) list)
  "Sets the list of portraits owned by the furre." ;; TODO portrait protocol
  (:function specitags ((furre furre)) list)
  "Returns the list of specitags owned by the furre." ;; TODO specitag protocol
  (:function (setf specitags) ((new-value list) (furre furre)) list)
  "Sets the list of specitags owned by the furre." ;; TODO specitag protocol
  (:function specitag-remap ((furre furre)) boolean)
  "Returns if the specitag should remap."
  (:function (setf specitag-remap) ((new-value boolean) (furre furre)) boolean)
  "Sets if the specitag should remap."
  (:function costumes ((furre furre)) list)
  "Returns a list of all costumes available on the furre."
  ;; TODO costume protocol
  (:function (setf costumes) ((new-value list) (furre furre)) list)
  "Sets a list of all costumes available on the furre."
  ;; TODO costume protocol
  )

(execute-protocol furre)
