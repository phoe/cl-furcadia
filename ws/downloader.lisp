;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; downloader.lisp

(in-package :cl-furcadia/ws)

(defparameter *digodata-url*
  "https://cms.furcadia.com/fured/digodata.js")

(defparameter *fox-download-url*
  "https://cms.furcadia.com/fured/fox5/species~D.fox~^?~D")

(defun http-get-digodata ()
  (let* ((response (get-url *digodata-url* :want-stream t))
         (parsed (parse-js:parse-js response))
         (vars (cadr parsed))
         (digodata (find "DIGODATA" vars :test #'string= :key #'caaadr)))
    ;; God forgive me for this function call below
    (mapcar (lambda (y) (mapcar (lambda (x) (cons (car x) (cddr x))) y))
            (mapcar #'cadr (caddr (caadr digodata))))))

(defun ensure-digo (name index)
  (multiple-value-bind (result foundp) (gethash index *digos*)
    (if foundp
        result
        (setf (gethash index *digos*)
              (make-instance 'standard-digo :name name :index index)))))

(defun update-digo-data ()
  "Fetches the current digo data from the server and updates it locally.
Returns the list of digo indices whose version was updated."
  (flet ((sfind (entry x) (second (find x entry :key #'car :test #'string=))))
    (let ((new-data (http-get-digodata)))
      (loop for entry in new-data
            for name = (sfind entry "n")
            for index = (sfind entry "s")
            for version = (sfind entry "v")
            for freep = (sfind entry "d")
            for exclusivep = (sfind entry "x")
            for alternate-form = (sfind entry "e")
            for wingablep = (if (find index *wingable-digos*) t nil)
            for digo = (ensure-digo name index)
            for old-version = (version digo)
            if (eq version :false)
              do (setf version nil)
            do (setf (version digo) version
                     (freep digo) freep
                     (exclusivep digo) exclusivep
                     (alternate-form digo) alternate-form
                     (wingablep digo) wingablep)
            unless (eql version old-version)
              collect index))))

(defun download-official-fox (id pathname)
  "Downloads an official FOX5 file with the given ID from the Furcadia Web
Services."
  (let ((url (format nil *fox-download-url* id)))
    (trivial-download:download url pathname :quiet t)))
