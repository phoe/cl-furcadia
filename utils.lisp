;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package #:cl-furcadia)

(defun read-data-file (pathname)
  "Reads the data file from the provided pathname. The pathname should be
a system relative pathname."
  (let ((full-pathname (asdf:system-relative-pathname :cl-furcadia pathname)))
    (with-input-from-file (stream full-pathname) (read stream))))

(defun from-220 (char)
  "Converts a character into a Furcadia base-220 integer."
  (let ((code (- (char-code char) 35)))
    (if (< 0 code 220)
        code
        0)))

(defun to-220 (number)
  "Converts a Furcadia base-220 integer into a character."
  (if (< 0 number 220)
      (code-char (+ number 35))
      #\#))
