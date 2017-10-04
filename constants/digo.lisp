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

(defvar *wingable-digos*
  '(1 2 3 4 5 6 7 8 9 10 96 120 121 127 131 132 138 149 159 188 228 234 257)
  "Identifiers for digos which are capable of displaying wings.")

(defun wings-name (wings)
  "Given a wing type, returns it proper name in form of a string."
  (check-type wings (or null keyword))
  (if (null wings)
      "No Wings"
      (let* ((name (string-capitalize (string wings))))
        (cat name " Wings"))))

(defgeneric wingable-digo-p (object)
  (:documentation #.(format nil "Returns true if the provided digo is ~
wingable, false otherwise.")))

(defmethod wingable-digo-p ((object integer))
  (multiple-value-bind (digo foundp) (gethash object *digos*)
    (if foundp
        (wingable-digo-p digo)
        (error "Digo with ID ~D was not found." object))))

(defmethod wingable-digo-p ((digo digo))
  (let ((index (index digo)))
    (if (member index *wingable-digos*) t nil)))
