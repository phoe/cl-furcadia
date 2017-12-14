;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; account.lisp

(in-package #:cl-furcadia/clos)

(defclass account ()
  ((%email :reader email
           :initarg :email)
   (%id :accessor id
        :initarg :id)
   (%main :accessor main
          :initarg :main)
   (%gd :accessor gd
        :initarg :gd
        :initform 0)
   (%furres :accessor furres
            :initarg :furres
            :initform '())))

(define-readable-print (account stream :identity nil)
  (format stream "(~A)" (email account)))
