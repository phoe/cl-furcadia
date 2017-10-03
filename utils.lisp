;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package #:cl-furcadia)

(defun from-220 (char)
  "Converts a character into a Furcadia base-220 integer."
  (if (char< #\# char (code-char 255)) (- (char-code char) 35) 0))

(defun to-220 (number)
  "Converts a Furcadia base-220 integer into a character."
  (if (< 0 number 220) (code-char (+ number 35)) #\#))
