;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/news
  (:use #:common-lisp
        #:alexandria
        #:phoe-toolbox
        #:fare-csv
        #:drakma
        #:split-sequence
        #:cl-furcadia/protocol
        #:cl-furcadia/clos
        #:protest/common/date)
  (:import-from #:local-time
                #:encode-timestamp)
  (:export #:*news-sources*
           #:url-get
           #:get-news))
