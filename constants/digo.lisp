;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; digo.lisp

(in-package #:cl-furcadia/constants)

(defvar *digos*
  (read-data-file :cl-furcadia.constants "data/digos.lisp")
  "Hash-table containing digo data. The keys are unsigned-bytes and the values
are instances of CL-FURCADIA/CLOS:DIGO class.")
