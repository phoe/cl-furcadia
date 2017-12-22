;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; furre.lisp

(in-package :cl-furcadia/protocol)

;;; TODO setf, setf everywhere
(define-protocol furre
    (:description "The FURRE protocol describes objects representing playable ~
characters in MMOSG Furcadia; they are called furres. These characters are ~
able to be logged into the game, post messages for other characters to see and ~
their players to read, change their appearances, etc..
\
Each furre belongs to an account.
\
Indirect instances of this class should be returned every time client code ~
successfully accesses Furcadia WS to retrieve characters from the servers."
     :tags (:cl-furcadia :furre)
     :export t)
  (:class furre () ())
  "A furre object. Each class participating in this protocol must subclass ~
this protocol class."
  (:function name ((furre furre)) string)
  "Returns the furre's name."
  (:function shortname ((furre furre)) string)
  "Returns the furre's shortname, computed from the furre's name."
  (:function uid ((furre furre)) unsigned-byte)
  "Returns the furre's UID."
  (:function last-login ((furre furre)) unsigned-byte)
  "Returns the date when the furre was last logged in."
  (:function description ((furre furre)) string)
  "Returns the furre's description."
  (:function color-code ((furre furre)) string)
  "Returns the furre's color code." ;; TODO color code protocol?
  (:function digo ((furre furre)) unsigned-byte)
  "Returns the furre's digo." ;; TODO return DIGO instead
  (:function wings ((furre furre)) unsigned-byte)
  "Returns the furre's wings." ;; TODO return WINGS instead
  (:function portrait ((furre furre)) unsigned-byte)
  "Returns the furre's portrait ID." ;; TODO make it possible to download ports
  (:function tag ((furre furre)) unsigned-byte)
  "Returns the furre's specitag ID." ;; TODO make it possible to download tags
  (:function auto-response ((furre furre)) string)
  "Returns the furre's auto-response message."
  (:function auto-response-p ((furre furre)) boolean)
  "Returns true if the furre auto-responds to people whispering them, and ~
false otherwise."
  (:function afk-description ((furre furre)) string)
  "Returns the furre's AFK description."
  (:function afk-whisper ((furre furre)) string)
  "Returns the furre's AFK auto-response."
  (:function afk-color-code ((furre furre)) string)
  "Returns the furre's AFK color code." ;; TODO color code protocol?
  (:function afk-digo ((furre furre)) unsigned-byte)
  "Returns the furre's AFK digo." ;; TODO return DIGO instead
  (:function afk-wings ((furre furre)) unsigned-byte)
  "Returns the furre's wings." ;; TODO return WINGS instead
  (:function afk-time ((furre furre)) unsigned-byte)
  "Returns the time, after which the furre becomes AFK."
  (:function afk-max-time ((furre furre)) unsigned-byte)
  "Returns the time, after which the furre disconnects."
  (:function digos ((furre furre)) list)
  "Returns the list of digos owned by the furre." ;; TODO return DIGOs
  (:function lifers ((furre furre)) list)
  "Returns the list of digos owned by the furre for life." ;; TODO return DIGOs
  (:function portraits ((furre furre)) list)
  "Returns the list of portraits owned by the furre." ;; TODO portrait protocol
  (:function specitags ((furre furre)) list)
  "Returns the list of specitags owned by the furre." ;; TODO specitag protocol
  (:function specitag-remap ((furre furre)) string)
  "Returns the furre's color code used for remapping specitags."
  ;; TODO color code protocol
  (:function costumes ((furre furre)) list)
  "Returns a list of all costumes available on the furre."
  ;; TODO costume protocol
  )
