;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.constants+clos.asd

(asdf:defsystem #:cl-furcadia.constants+clos
  :description "Constant values for interacting with Furcadia - CLOS part."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:phoe-toolbox
               #:cl-furcadia.base
               #:cl-furcadia.constants
               #:cl-furcadia.clos)
  :serial t
  :components ((:file "package")
               (:file "digo")))
