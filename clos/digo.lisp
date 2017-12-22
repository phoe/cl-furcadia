;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; digo.lisp

(in-package #:cl-furcadia/clos)

(defclass digo ()
  ((%index :reader index
           :initarg :index)
   (%name :reader name
          :initarg :name
          :initform "")
   (%version :accessor version
             :initarg :version
             :initform nil)
   (%freep :accessor freep
           :initarg freep
           :initform nil)
   (%exclusivep :accessor exclusivep
                :initarg :exclusivep
                :initform nil)
   (%alternate-form :accessor alternate-form
                    :initarg :alternate-form
                    :initform nil)))

(define-readable-print (digo stream :identity nil)
  (format stream "~A (~D)" (name digo) (index digo)))
