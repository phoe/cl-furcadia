;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; constants.lisp

(in-package #:cl-furcadia/remap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *color-types*
    '(:fur :markings :hair :eyes :badge :vest :bracers
      :cape :boots :trousers :wings :accent)
    "All valid remappable color types in a Furcadia (sans outline).")

  (defun read-data-file (pathname)
    "Reads the data file from the provided pathname. The pathname should be
a system relative pathname."
    (let ((full-pathname (asdf:system-relative-pathname :cl-furcadia.remap
                                                        pathname)))
      (with-input-from-file (stream full-pathname) (read stream)))))

(deftype color ()
  "A symbol denoting a Furcadia color type."
  '#.`(member ,@*color-types*))

(defvar *gradients* (read-data-file "data/gradients.lisp")
  "Hash-table containing gradient data. The keys are in form (SYMBOL STRING),
where SYMBOL is taken from *COLOR-TYPES* and STRING is a valid color name.")

(defvar *color-names* (read-data-file "data/color-names.lisp")
  "Hash-table containing color names. Keys are symbols from *COLOR-TYPES*.")

(defvar *color-values* (read-data-file "data/color-values.lisp")
  "Hash table between integers and their respective remap types.")

(defvar *color-code-indices*
  '(:version :fur :markings :hair :eyes :badge :vest :bracers :cape
    :boots :trousers :wings :accent :gender :species :reserved)
  "Indices of a Furcadia color code.")

(defvar *genders*
  '(:female :male :unspecified)
  "Gender values possible in a Furcadia color code.")
