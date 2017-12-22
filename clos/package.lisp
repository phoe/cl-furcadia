;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:cl-furcadia/clos
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:cl-furcadia/base
   #:cl-furcadia/protocol)
  (:export
   #:standard-digo
   #:standard-furre
   #:standard-account
   #:standard-news)
  ;; (:export
  ;;  ;; Furre class and accessors
  ;;  #:furre
  ;;  #:name #:shortname #:uid #:description #:color-code #:digo #:wings #:portrait
  ;;  #:tag #:auto-response #:auto-response-p #:afk-description #:afk-whisper
  ;;  #:afk-color-code #:afk-digo #:afk-wings #:afk-portrait #:afk-time
  ;;  #:afk-max-time #:digos #:lifers #:portraits #:specitags #:specitag-remap
  ;;  #:costumes #:last-login
  ;;  ;; Digo class and accessors
  ;;  #:digo
  ;;  #:index #:name #:version #:freep #:exclusivep #:alternate-form
  ;;  ;; News class and acccessors
  ;;  #:news
  ;;  #:title #:contents #:category #:date #:datestring #:url #:image-url
  ;;  #:image-filename
  ;;  ;; Account class and accessors
  ;;  #:account
  ;;  #:email #:id #:main #:gd #:furres #:session
  ;;  )
  )
