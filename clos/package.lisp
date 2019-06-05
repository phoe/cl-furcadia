;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/clos
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:protest/common/date
   #:cl-furcadia/base
   #:cl-furcadia/protocol)
  (:export
   #:standard-digo
   #:standard-furre
   #:standard-costume
   #:standard-account
   #:standard-portrait
   #:standard-news
   #:standard-specitag
   #:standard-date #:now
   #:standard-image
   #:standard-post))
