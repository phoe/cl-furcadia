;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/news
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:fare-csv
   #:drakma
   #:split-sequence
   #:cl-furcadia/protocol
   #:cl-furcadia/clos)
  (:import-from #:local-time #:encode-timestamp #:timestamp>)
  (:export
   #:*news-sources*
   #:get-news))
