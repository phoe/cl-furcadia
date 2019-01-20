;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-furre.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-furre (furre)
  ((%uid :accessor uid
         :initarg :uid
         :initform 0)
   (%name :accessor name
          :initarg :name
          :initform "")
   (%last-login :accessor last-login
                :initarg :last-login
                :initform 0)
   (%digos :accessor digos
           :initarg :digos
           :initform '())
   (%lifers :accessor lifers
            :initarg :lifers
            :initform '())
   (%images :accessor images
            :initarg :images
            :initform '())
   (%portraits :accessor portraits
               :initarg :portraits
               :initform '())
   (%specitags :accessor specitags
               :initarg :specitags
               :initform '())
   (%costumes :accessor costumes
              :initarg :costumes
              :initform '())
   (%active-costume :writer (setf active-costume)
                    :initform nil)
   (%account :accessor account
             :initarg :account
             :initform nil)))

(defmethod shortname ((furre standard-furre))
  (name-shortname (name furre)))

(define-readable-print (standard-furre stream :identity nil)
  (format stream "~S" (name standard-furre)))

(defmethod active-costume ((furre standard-furre))
  (or (slot-value furre '%active-costume)
      (find -1 (costumes furre) :key #'cid)
      (error "The furre has no costumes.")))
