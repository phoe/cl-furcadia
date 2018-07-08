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
     :dependencies (named)
     :export t)
  ;; TODO adjust exports
  (:class costume (named) ())
  "A costume object. Each class participating in the protocol must subclass ~
the protocol class."
  (:function cid ((costume costume)) unsigned-byte)
  "Returns the costume's CID. The value should be provided when instantiating ~
the costume and is otherwise immutable."
  (:function furre ((costume costume)) (or furre null))
  "Returns the furre the costume belongs to, or NIL if the costume is not ~
associated with a furre."
  (:function (setf furre) ((new-value (or furre null)) (costume costume))
             (or furre null))
  "Sets the furre the costume belongs to."
  (:function rating ((costume costume)) keyword)
  "Returns the rating of the costume."
  (:function (setf rating) ((new-value keyword) (costume costume)) keyword)
  "Sets the rating of the costume."
  (:function scale ((costume costume)) unsigned-byte)
  "Returns the scale of the costume."
  (:function (setf scale) ((new-value unsigned-byte) (costume costume))
             unsigned-byte)
  "Sets the scale of the costume."
  (:function ordinal ((costume costume)) unsigned-byte)
  "Returns the orginal of the costume on the costume list of a costume."
  (:function (setf ordinal) ((new-value unsigned-byte) (costume costume))
             unsigned-byte)
  "Sets the ordinal of the costume on the costume list of a costume."
  (:function description ((costume costume)) string)
  "Returns the costume's description."
  (:function (setf description) ((new-value string) (costume costume)) string)
  "Sets the costume's description."
  (:function color-code ((costume costume)) string)
  "Returns the costume's color code." ;; TODO color code protocol?
  (:function (setf color-code) ((new-value string) (costume costume)) string)
  "Sets the costume's color code." ;; TODO color code protocol?
  (:function specitag ((costume costume)) unsigned-byte)
  "Returns the costume's specitag ID."
  ;; TODO make it possible to download tags
  (:function (setf specitag) ((new-value unsigned-byte) (costume costume))
             unsigned-byte)
  "Sets the costume's specitag ID." ;; TODO make it possible to download tags
  (:function digo ((costume costume)) unsigned-byte)
  "Returns the costume's digo." ;; TODO return DIGO instead
  (:function (setf digo)
             ((new-value unsigned-byte) (costume costume)) unsigned-byte)
  "Sets the costume's digo." ;; TODO return DIGO instead
  (:function wings ((costume costume)) unsigned-byte)
  "Returns the costume's wings." ;; TODO return WINGS instead
  (:function (setf wings)
             ((new-value unsigned-byte) (costume costume)) unsigned-byte)
  "Sets the costume's wings." ;; TODO return WINGS instead
  (:function portrait ((costume costume)) unsigned-byte)
  "Returns the costume's portrait ID."
  ;; TODO make it possible to download ports
  (:function (setf portrait)
             ((new-value unsigned-byte) (costume costume)) unsigned-byte)
  "Sets the costume's portrait ID."
  ;; TODO make it possible to download ports
  (:function auto-response ((costume costume)) string)
  "Returns the costume's auto-response message."
  (:function (setf auto-response) ((new-value string) (costume costume)) string)
  "Sets the costume's auto-response message."
  (:function auto-response-p ((costume costume)) boolean)
  "Returns true if the costume auto-responds to people whispering them, and ~
false otherwise."
  (:function (setf auto-response-p) ((new-value boolean) (costume costume))
             boolean)
  "Returns true if the costume auto-responds to people whispering them, and ~
false otherwise."
  (:function afk-description ((costume costume)) string)
  "Returns the costume's AFK description."
  (:function (setf afk-description) ((new-value string) (costume costume))
             string)
  "Sets the costume's AFK description."
  (:function afk-portrait ((costume costume)) unsigned-byte)
  "Returns the costume's AFK portrait."
  (:function (setf afk-portrait)
             ((unsigned-byte new-value) (costume costume)) unsigned-byte)
  "Sets the costume's AFK portrait."
  (:function afk-whisper ((costume costume)) string)
  "Returns the costume's AFK auto-response."
  (:function (setf afk-whisper) ((new-value string) (costume costume)) string)
  "Sets the costume's AFK auto-response."
  (:function afk-color-code ((costume costume)) string)
  "Returns the costume's AFK color code." ;; TODO color code protocol?
  (:function (setf afk-color-code) ((new-value string) (costume costume))
             string)
  "Sets the costume's AFK color code." ;; TODO color code protocol?
  (:function afk-digo ((costume costume)) unsigned-byte)
  "Returns the costume's AFK digo." ;; TODO return DIGO instead
  (:function (setf afk-digo)
             ((new-value unsigned-byte) (costume costume)) unsigned-byte)
  "Sets the costume's AFK digo." ;; TODO return DIGO instead
  (:function afk-wings ((costume costume)) unsigned-byte)
  "Returns the costume's wings." ;; TODO return WINGS instead
  (:function (setf afk-wings)
             ((new-value unsigned-byte) (costume costume)) unsigned-byte)
  "Setf the costume's wings." ;; TODO return WINGS instead
  (:function afk-time ((costume costume)) unsigned-byte)
  "Returns the time, after which the costume becomes AFK."
  (:function (setf afk-time)
             ((new-value unsigned-byte) (costume costume)) unsigned-byte)
  "Sets the time, after which the costume becomes AFK."
  (:function afk-max-time ((costume costume)) unsigned-byte)
  "Returns the time, after which the costume disconnects."
  (:function (setf afk-max-time)
             ((new-value unsigned-byte) (costume costume)) unsigned-byte)
  "Sets the time, after which the costume disconnects.")

(execute-protocol costume)
