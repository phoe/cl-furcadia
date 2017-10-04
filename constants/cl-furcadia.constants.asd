;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.constants.asd

(asdf:defsystem #:cl-furcadia.constants
  :description "Constant values for interacting with Furcadia."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:phoe-toolbox
               #:cl-furcadia
               #:cl-furcadia.clos)
  :serial t
  :components ((:file "package")
               (:file "digo")
               (:file "remap")))
