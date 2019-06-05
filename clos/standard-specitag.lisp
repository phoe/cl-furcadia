;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-specitag.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-specitag (specitag)
  ((%sid :reader sid
         :initarg :sid
         :initform (error "Must provide SID."))
   (%remappedp :accessor remappedp
               :initarg :remappedp
               :initform nil)
   (%furre :accessor furre
           :initarg :furre
           :initform nil)))

(define-readable-print (standard-specitag stream :identity nil)
  (format stream "~D" (sid standard-specitag))
  (when-let ((furre (furre standard-specitag)))
    (format stream " (~A)" (name furre))))

(defmethod image-data ((specitag standard-specitag) dl-path)
  (let* ((sid-string (princ-to-string (sid specitag)))
         (specitags-dir (merge-pathnames "specitags/" dl-path))
         (sname (shortname (furre specitag)))
         (specitag-dir (merge-pathnames (uiop:strcat sname "/") specitags-dir))
         (data-path (merge-pathnames (uiop:strcat sid-string ".png")
                                     specitag-dir)))
    (when (probe-file data-path)
      (pngload:load-file data-path))))

;; TODO stop using internal package access when pngload exports that symbol
(defmethod (setf image-data)
    ((png pngload::png-object) (specitag standard-specitag) dl-path)
  (let* ((sid-string (princ-to-string (sid specitag)))
         (specitags-dir (merge-pathnames "specitags/" dl-path))
         (sname (shortname (furre specitag)))
         (specitag-dir (merge-pathnames (uiop:strcat sname "/") specitags-dir))
         (data-path (merge-pathnames (uiop:strcat sid-string ".png")
                                     specitag-dir)))
    (ensure-directories-exist specitag-dir)
    (let ((zpng (make-instance 'zpng:png :width (pngload:width png)
                                         :height (pngload:height png)
                                         :color-type :truecolor-alpha
                                         :image-data (pngload:data png))))
      (zpng:write-png zpng data-path)
      png)))
