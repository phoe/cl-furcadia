;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; cl-furcadia.news.asd

(asdf:defsystem #:cl-furcadia.news
  :description "News fetcher for Furcadia and alikes."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 3-clause"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:local-time
               #:fare-csv
               #:split-sequence
               #:drakma
               #:cl-furcadia.clos)
  :serial t
  :components ((:file "package")
               (:file "news")))
