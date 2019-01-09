;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; standard-image.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-image (image)
  ((%iid :accessor iid
         :initarg :iid
         :initarg :id
         :initform (required-argument :id))
   (%furre :accessor furre
           :initarg :furre
           :initform nil)
   (%timestamp :accessor timestamp
               :initarg :timestamp
               :initform 0)
   (%url :accessor url
         :initarg :url
         :initform "")
   (%eye-level :accessor eye-level
               :initarg :eye-level
               :initform 0.2)
   (%data :accessor data
          :initarg :data
          :initform nil)
   (%sfwp :accessor sfwp
          :initarg :sfwp
          :initarg :sfw
          :initform t)))

(define-readable-print (standard-image stream :identity nil)
  (format stream "~A ~A"
          (iid standard-image)
          (if (sfwp standard-image) :sfw :nsfw))
  (when-let ((furre (furre standard-image)))
    (format stream " (~D)" (shortname furre))))
