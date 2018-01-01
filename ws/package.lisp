;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/ws
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:cl-furcadia/constants
   #:cl-furcadia/protocol
   #:cl-furcadia/clos
   #:json)
  (:import-from #:cl-furcadia/news #:get-url)
  (:export
   #:login
   #:fetch-account
   #:fetch-furre
   #:save-furre))
