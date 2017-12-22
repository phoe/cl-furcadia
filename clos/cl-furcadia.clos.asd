;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.clos.asd

(asdf:defsystem #:cl-furcadia.clos
  :description "Class definitions for objects used in Furcadia."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:cl-furcadia.base)
  :serial t
  :components ((:file "package")
               (:file "digo")
               (:file "furre")
               (:file "account")
               (:file "news")))
