;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.remap.asd

(asdf:defsystem #:cl-furcadia.remap
  :description "Utilities for remapping Furcadia images."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:phoe-toolbox
               #:cl-furcadia.base
               #:cl-furcadia.constants)
  :serial t
  :components ((:file "package")
               (:file "remap")))
