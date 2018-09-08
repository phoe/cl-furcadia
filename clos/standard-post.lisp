;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; standard-post.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-post (post)
  ((%author-shortname :accessor author-shortname
                      :initarg :author-shortname
                      :initform nil)
   (%date :accessor date
          :initarg :date
          :initform (now))
   (%contents :accessor contents
              :initarg :contents
              :initform "")))

(define-readable-print (standard-post stream :identity nil)
  (format stream "~A (~D chars)"
          (author-shortname standard-post)
          (length (contents standard-post))))
