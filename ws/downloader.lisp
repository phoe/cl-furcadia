;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; downloader.lisp

(in-package :cl-furcadia/ws)

(defparameter *digodata-url*
  "https://cms.furcadia.com/fured/digodata.js")

(defun http-get-digodata ()
  (let* ((response (get-url *digodata-url* :want-stream t))
         (parsed (parse-js:parse-js response))
         (vars (cadr parsed))
         (digodata (find "DIGODATA" vars :test #'string= :key #'caaadr)))
    (mapcar (lambda (y) (mapcar (lambda (x) (cons (car x) (cddr x))) y))
            (mapcar #'cadr (caddr (caadr digodata))))))

;;; TODO finish
