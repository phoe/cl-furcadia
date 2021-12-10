;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; standard-image.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-image (image)
  ((%iid :accessor iid
         :initarg :iid
         :initarg :id
         :initform (required-argument :id))
   (%furre :accessor furre
           :initarg :furre
           :initform nil)
   (%timestamp :accessor timestamp
               :initarg :timestamp
               :initform 0)
   (%url :accessor url
         :initarg :url
         :initform "")
   (%eye-level :accessor eye-level
               :initarg :eye-level
               :initform 0.2)
   (%sfwp :accessor sfwp
          :initarg :sfwp
          :initarg :sfw
          :initform t)))

(define-readable-print (standard-image stream :identity nil)
  (format stream "~A ~A"
          (iid standard-image)
          (if (sfwp standard-image) :sfw :nsfw))
  (when-let ((furre (furre standard-image)))
    (format stream " (~D)" (name furre))))

(defmethod image-data ((image standard-image) dl-path)
  (let* ((timestamp-string (princ-to-string (timestamp image)))
         (images-dir (merge-pathnames "images/" dl-path))
         (sname (shortname (furre image)))
         (image-dir (merge-pathnames (uiop:strcat sname "/") images-dir))
         (data-path (merge-pathnames (uiop:strcat timestamp-string ".png")
                                     image-dir)))
    (when (probe-file data-path)
      (pngload:load-file data-path))))

;; TODO stop using internal package access when pngload exports that symbol
(defmethod (setf image-data)
    ((png pngload::png) (image standard-image) dl-path)
  (let* ((timestamp-string (princ-to-string (timestamp image)))
         (images-dir (merge-pathnames "images/" dl-path))
         (sname (shortname (furre image)))
         (image-dir (merge-pathnames (uiop:strcat sname "/") images-dir))
         (data-path (merge-pathnames (uiop:strcat timestamp-string ".dat")
                                     image-dir)))
    (ensure-directories-exist image-dir)
    (let ((zpng (make-instance 'zpng:png :width (pngload:width png)
                                         :height (pngload:height png)
                                         :color-type :truecolor-alpha
                                         :image-data (pngload:data png))))
      (zpng:write-png zpng data-path)
      png)))
