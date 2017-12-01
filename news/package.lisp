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
   #:cl-furcadia/clos)
  (:import-from #:local-time #:encode-timestamp #:timestamp>)
  (:export
   ;; Functions
   #:*news-sources*
   #:get-news
   ;; Reexport of news symbols from CL-FURCADIA/CLOS
   #:news
   #:title #:contents #:category #:date #:datestring #:url #:image-url
   #:image-filename))
