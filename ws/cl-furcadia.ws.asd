;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.ws.asd

(asdf:defsystem #:cl-furcadia.ws
  :description "Library for handling Furcadia web services."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:drakma
               #:cl-json
               #:cl-furcadia.clos)
  :serial t
  :components ((:file "package")
               (:file "ws")))