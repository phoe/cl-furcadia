;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package #:cl-furcadia/clos)

(defmacro check-boundp (object slot-name)
  "Asserts that the provided slot is bound."
  (let ((name (subseq (string slot-name) 1)))
    `(unless (slot-boundp ,object ',slot-name)
       (error "Must provide ~A." ,name))))
