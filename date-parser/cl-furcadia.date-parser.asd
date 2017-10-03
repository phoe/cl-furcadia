;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.date-parser.asd

(asdf:defsystem #:cl-furcadia.date-parser
  :description "Date parser for dates used in Furcadia news."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:esrap
               #:parser.common-rules)
  :serial t
  :components ((:file "date-parser")))
