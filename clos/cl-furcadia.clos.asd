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
               #:local-time
               #:lorem-ipsum ;; TODO remove when in production
               #:cl-furcadia.base
               #:cl-furcadia.protocol)
  :serial t
  :components ((:file "package")
               (:file "standard-digo")
               (:file "standard-costume")
               (:file "standard-furre")
               (:file "standard-account")
               (:file "standard-portrait")
               (:file "standard-date")
               (:file "standard-image")
               (:file "standard-news")
               (:file "standard-post")))
