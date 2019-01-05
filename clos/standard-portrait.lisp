;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-portrait.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-portrait (costume)
  ((%pid :accessor pid
         :initarg :pid
         :initform 0)
   (%furre :accessor furre
           :initarg :furre
           :initform nil)
   (%portrait-type :accessor portrait-type
                   :initarg :portrait-type
                   :initform :8-bit)
   (%remappedp :accessor remappedp
               :initarg :remappedp
               :initform nil)
   (%data :accessor data
          :initarg :data
          :initform #())))

(define-readable-print (standard-portrait stream :identity nil)
  (format stream "~S, ~A, ~A bytes"
          (portrait-type standard-portrait)
          (if (remappedp standard-portrait) "remappable" "non-remappable")
          (length (data standard-portrait)))
  (when-let ((furre (furre standard-portrait)))
    (format stream " (~S)" (name furre))))
