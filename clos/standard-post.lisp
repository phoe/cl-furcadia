;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; standard-post.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-post (post)
  ((%shortname :accessor shortname
               :initarg :shortname
               :initform nil)
   (%date :accessor date
          :initarg :date
          :initform (now))
   (%contents :accessor contents
              :initarg :contents
              :initform "")))

(define-readable-print (standard-post stream :identity nil)
  (format stream "~A (~D chars)"
          (or (shortname standard-post) "(anonymous)")
          (length (contents standard-post))))
