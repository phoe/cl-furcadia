;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; account.lisp

(in-package #:cl-furcadia/clos)

(defclass account ()
  ((%email :reader email
           :initarg :email)
   (%password :writer password
              :initarg :password)
   (%id :accessor id
        :initarg :id)
   (%main :accessor main
          :initarg :main)
   (%gd :accessor gd
        :initarg :gd
        :initform 0)
   (%furres :accessor furres
            :initarg :furres
            :initform '())
   (%session :accessor session
             :initarg :session
             :initform nil)))

(define-constructor (account json-characters)
  (when (and json-characters (not (slot-boundp account '%furres)))
    (setf (furres account)
          (mapcar #'character-furre json-characters))))

(define-readable-print (account stream :identity nil)
  (format stream "~A" (email account)))