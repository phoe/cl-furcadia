;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-digo.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-digo (digo)
  ((%index :reader index
           :initarg :index
           :initform (error "Must provide DIGO."))
   (%name :reader name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%version :accessor version
             :initarg :version
             :initform 0)
   (%freep :accessor freep
           :initarg :freep
           :initform nil)
   (%exclusivep :accessor exclusivep
                :initarg :exclusivep
                :initform nil)
   (%alternate-form :accessor alternate-form
                    :initarg :alternate-form
                    :initform nil)
   (%wingablep :accessor wingablep
               :initarg :wingablep
               :initform nil)
   (%fox-file :accessor fox-file
              :initarg :fox-file
              :initform nil)))

(define-readable-print (standard-digo stream :identity nil)
  (format stream "~A (~D)" (name standard-digo) (index standard-digo)))
