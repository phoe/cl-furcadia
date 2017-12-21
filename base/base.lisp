;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(defpackage #:cl-furcadia/base
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox)
  (:export
   #:from-220
   #:to-220
   #:name-shortname))

(in-package #:cl-furcadia/base)

(defun from-220 (char)
  "Converts a character into a Furcadia base-220 integer."
  (if (char< #\# char (code-char 255)) (- (char-code char) 35) 0))

(defun to-220 (number)
  "Converts a Furcadia base-220 integer into a character."
  (if (< 0 number 220) (code-char (+ number 35)) #\#))

(defvar *shortname-char-table*
  #(nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    #\0  #\1  #\2  #\3  #\4  #\5  #\6  #\7
    #\8  #\9  nil  nil  nil  nil  nil  nil
    nil  #\a  #\b  #\c  #\d  #\e  #\f  #\g
    #\h  #\i  #\j  #\k  #\l  #\m  #\n  #\o
    #\p  #\q  #\r  #\s  #\t  #\u  #\v  #\w
    #\x  #\y  #\z  nil  nil  nil  nil  nil
    nil  #\a  #\b  #\c  #\d  #\e  #\f  #\g
    #\h  #\i  #\j  #\k  #\l  #\m  #\n  #\o
    #\p  #\q  #\r  #\s  #\t  #\u  #\v  #\w
    #\x  #\y  #\z  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    nil  nil  nil  nil  nil  nil  nil  nil
    #\a  #\a  #\a  #\a  #\a  #\a  #\a  #\c
    #\e  #\e  #\e  #\e  #\i  #\i  #\i  #\i
    #\d  #\n  #\o  #\o  #\o  #\o  #\o  nil
    #\o  #\u  #\u  #\u  #\u  #\y  nil  nil
    #\a  #\a  #\a  #\a  #\a  #\a  #\a  #\c
    #\e  #\e  #\e  #\e  #\i  #\i  #\i  #\i
    #\o  #\n  #\o  #\o  #\o  #\o  #\o  nil
    #\o  #\u  #\u  #\u  #\u  #\y  nil  #\y)
  "Table using for converting names into shortnames.")

(defun name-shortname (string)
  "Given a Furcadia name, returns a shortname matching that name."
  (loop for char across string
        for code = (char-code char)
        for new-char = (aref *shortname-char-table* code)
        when new-char
          collect new-char into result
        finally (return (coerce result 'string))))
