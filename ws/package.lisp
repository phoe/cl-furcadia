;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/ws
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:trivial-download
   #:cl-furcadia/constants
   #:cl-furcadia/protocol
   #:cl-furcadia/clos
   #:json)
  (:import-from #:cl-furcadia/news #:get-url)
  (:export
   #:login
   #:fetch-account
   #:fetch-furre
   #:fetch-costume
   #:fetch-portrait
   #:save-furre
   #:ensure-digo
   #:update-digo-data
   #:download-official-fox))
