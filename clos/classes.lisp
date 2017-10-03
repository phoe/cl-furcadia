;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package #:cl-furcadia/clos)

(defclass furre ()
  (name
   shortname
   uid
   description
   color-code
   digo
   wings
   portrait
   tag
   whisper-response
   afk-description
   afk-whisper
   afk-whisper-response
   afk-color-code
   afk-digo
   afk-wings
   afk-portrait
   afk-time
   afk-max-time))
