;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; legacy.lisp

(in-package #:cl-furcadia/remap)

(defvar *legacy-remaps* (make-hash-table)
  "Hashtable from legacy remap colors to legacy remap types. Keys are indices on
the classic palette. Values are two-element lists in form of (KEYWORD INDEX),
where KEYWORD is a legacy remap type and INDEX is an index on this color's
brightness (0 is brightest).")

(defmacro define-legacy-remaps (&body body)
  `(setf ,@(loop for (index keyword count) in body
                 for iota = (iota count)
                 for indices = (mapcar (curry #'+ index) iota)
                 nconc (loop for i in iota
                             for index in indices
                             nconc `((gethash ,index *legacy-remaps*)
                                     '(,keyword ,i))))))

(define-legacy-remaps
  (10 :shadow 1) (11 :badge 1) (16 :boots 8) (32 :cape 8) (50 :eyes 1)
  (72 :vest 8) (80 :bracers 8) (128 :hair 8) (136 :markings 8) (199 :fur 8)
  (207 :outline 1) (224 :trousers 8))
