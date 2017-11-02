;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; constants.lisp

(in-package #:cl-furcadia/base)

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
