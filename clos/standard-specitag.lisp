;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-specitag.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-specitag (specitag)
  ((%index :reader index
           :initarg :index
           :initform (error "Must provide SPECITAG."))
   (%data :accessor data
          :initarg :data
          :initform #())
   (%remappedp :accessor remappedp
               :initarg :remappedp
               :initform nil)
   (%furre :accessor furre
           :initarg :furre
           :initform nil)))

(define-readable-print (standard-specitag stream :identity nil)
  (format stream "~D" (index standard-specitag))
  (when-let ((furre (furre standard-specitag)))
    (format stream " (~A)" (name furre))))
