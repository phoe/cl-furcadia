;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.base.asd

(asdf:defsystem #:cl-furcadia.base
  :description "Library of utils for MMOSG Furcadia"
  :author "Michał \"phoe\" Herda <phoe@teknik.io>"
  :license "BSD 3-clause"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox)
  :components ((:file "package")
               (:file "constants")
               (:file "utils")))
