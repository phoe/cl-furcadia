;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.characters.asd

(asdf:defsystem #:cl-furcadia.characters
  :description "Character handling library for Furcadia."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:drakma
               #:cl-furcadia.clos)
  :serial t
  :components ((:file "package")
               (:file "characters")))
