;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-account.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-account (account)
  ((%email :reader email
           :initarg :email)
   (%password :accessor password
              :initarg :password
              :initform "")
   (%id :accessor id
        :initarg :id
        :initform 0)
   (%main :accessor main
          :initarg :main
          :initform nil)
   (%gd :accessor gd
        :initarg :gd
        :initform 0)
   (%furres :accessor furres
            :initarg :furres
            :initform '())
   (%session :accessor session
             :initarg :session
             :initform nil)
   (%cookie-jar :accessor cookie-jar-of
                :initarg :cookie-jar
                :initform nil)))

(define-constructor (standard-account)
  (unless (slot-boundp standard-account '%email)
    (error "Must provide EMAIL."))
  ;; TODO what is #'CHARACTER-FURRE?
  ;; (when (and json-characters (not (slot-boundp account '%furres)))
  ;;   (setf (furres standard-account)
  ;;         (mapcar #'character-furre json-characters)))
  )

(define-readable-print (standard-account stream :identity nil)
  (format stream "~A" (email standard-account)))
