;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; digo.lisp

(in-package #:cl-furcadia/clos)

(defclass digo ()
  ((%index :reader index
           :initarg :index)
   (%name :reader name
          :initarg :name)
   (%version :accessor version
             :initarg :version
             :initform nil)
   (%freep :reader freep
           :initarg freep
           :initform nil)
   (%exclusivep :reader exclusivep
                :initarg :exclusivep
                :initform nil)
   (%alternate-form :reader alternate-form
                    :initarg :alternate-form
                    :initform nil)))

(define-constructor (digo)
  ;; Index: must be provided and an unsigned-byte.
  (check-boundp digo %index)
  (assert (typep (index digo) 'unsigned-byte))
  ;; Name: must be provided and a string.
  (check-boundp digo %name)
  (assert (typep (name digo) 'string))
  ;; Version: must be an unsigned-byte.
  (when (slot-boundp digo '%version)
    (check-type (version digo) (or null unsigned-byte)))
  ;; Alternate form: must be an unsigned-byte.
  (when (slot-boundp digo '%alternate-form)
    (assert (typep (alternate-form digo) '(or null unsigned-byte)))))

(defmethod (setf version) :before (new-value (object digo))
  (assert (typep new-value 'unsigned-byte)))

(define-readable-print (digo stream :identity nil)
  (format stream "~A (~D)" (name digo) (index digo)))
