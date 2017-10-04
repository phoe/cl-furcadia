;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/clos
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:cl-furcadia)
  (:export
   ;; Furre class and accessors
   #:furre
   #:name #:shortname #:uid #:description #:color-code #:digo #:wings #:portrait
   #:tag #:whisper-response #:afk-description #:afk-whisper #:afk-color-code
   #:afk-whisper-response #:afk-digo #:afk-wings #:afk-portrait #:afk-time
   #:afk-max-time #:digos #:lifers #:portraits #:specitags #:specitag-remap
   #:costumes
   ;; Digo class and accessors
   #:digo
   #:index #:name #:version #:freep #:exclusivep #:alternate-form
   ))
