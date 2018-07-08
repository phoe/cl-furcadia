;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.protocol.asd

(asdf:defsystem #:cl-furcadia.protocol
  :description "Protocols for objects used in Furcadia."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:protest)
  :serial t
  :components ((:file "package")
               (:file "account")
               (:file "costume")
               (:file "furre")
               (:file "digo")
               (:file "news")))
