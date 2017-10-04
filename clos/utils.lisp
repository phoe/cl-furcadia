;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package #:cl-furcadia/clos)

(defmacro check-boundp (object slot-name)
  "Asserts that the provided slot is bound."
  (let* ((name (string slot-name))
         (result (if (char= (aref name 0) #\%) (subseq name 1) name)))
    `(unless (slot-boundp ,object ',slot-name)
       (error "Must provide ~A." ,result))))
