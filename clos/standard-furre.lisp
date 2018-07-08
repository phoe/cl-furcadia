;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; furre.lisp

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
   (%portraits :accessor portraits
               :initarg :portraits
               :initform '())
   (%specitags :accessor specitags
               :initarg :specitags
               :initform '())
   (%specitag-remap :accessor specitag-remap
                    :initarg :specitag-remap
                    :initform nil)
   (%costumes :accessor costumes
              :initarg :costumes
              :initform '())))

(defmethod shortname ((furre standard-furre))
  (name-shortname (name furre)))

(define-readable-print (standard-furre stream :identity nil)
  (format stream "~S" (name standard-furre)))
