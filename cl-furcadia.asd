;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.asd

(asdf:defsystem #:cl-furcadia
  :description "Library of utils for MMOSG Furcadia."
  :author "Michał \"phoe\" Herda <phoe@teknik.io>"
  :license "BSD 3-clause"
  :serial t
  :depends-on
  (#:alexandria
   #:phoe-toolbox
   #:drakma
   #:trivial-download
   #:cl-furcadia.base
   #:cl-furcadia.constants
   #:cl-furcadia.clos
   #:cl-furcadia.constants+clos
   #:cl-furcadia.date-parser
   #:cl-furcadia.news
   #:cl-furcadia.remap)
  :components ((:file "package")))
