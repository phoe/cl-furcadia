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

(defun name-shortname (string)
  "Given a Furcadia name, returns a shortname matching that name."
  (loop for char across string
        for code = (char-code char)
        for new-char = (aref *shortname-char-table* code)
        when new-char
          collect new-char into result
        finally (return (coerce result 'string))))

(defun read-data-file (system pathname)
  "Reads the data file from the provided pathname. The pathname should be
a system relative pathname."
  (let ((full-pathname (asdf:system-relative-pathname system pathname)))
    (with-input-from-file (stream full-pathname)
      (with-standard-io-syntax (read stream)))))
