;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; digo.lisp

(in-package #:cl-furcadia/constants)

(defvar *digos*
  (read-data-file :cl-furcadia.constants "data/digos.lisp")
  "Hash-table containing digo data. The keys are unsigned-bytes and the values
are instances of CL-FURCADIA/CLOS:DIGO class.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *wings*
    '(nil :classic :tri :butterfly :bat :prime :dragonfly)
    "Indices of Furcadia wing types."))

(deftype wing ()
  "A symbol denoting a Furcadia wing type."
  '#.`(member ,@*wings*))

(defun wings-name (wings)
  "Given "
  (check-type wings (or null keyword))
  (if (null wings)
      "No Wings"
      (let* ((name (string-capitalize (string wings))))
        (cat name " Wings"))))
