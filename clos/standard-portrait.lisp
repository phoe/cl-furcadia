;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-portrait.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-portrait (costume)
  ((%pid :accessor pid
         :initarg :pid
         :initform 0)
   (%furre :accessor furre
           :initarg :furre
           :initform nil)
   (%portrait-type :accessor portrait-type
                   :initarg :portrait-type
                   :initform :8-bit)
   (%remappedp :accessor remappedp
               :initarg :remappedp
               :initform nil)))

(define-readable-print (standard-portrait stream :identity nil)
  (format stream "~D ~S, ~A"
          (pid standard-portrait)
          (portrait-type standard-portrait)
          (if (remappedp standard-portrait) "remappable" "non-remappable"))
  (when-let ((furre (furre standard-portrait)))
    (format stream " (~A)" (name furre))))

(defmethod image-data ((portrait standard-portrait) dl-path)
  (let* ((pid-string (princ-to-string (pid portrait)))
         (portraits-dir (merge-pathnames "portraits/" dl-path))
         (sname (shortname (furre portrait)))
         (portrait-dir (merge-pathnames (uiop:strcat sname "/") portraits-dir))
         (data-path (merge-pathnames (uiop:strcat pid-string ".dat")
                                     portrait-dir)))
    (read-file-into-byte-vector data-path)))

;; TODO stop using internal package access when pngload exports that symbol
(defmethod (setf image-data)
    ((data vector) (portrait standard-portrait) dl-path)
  (check-type data (vector (unsigned-byte 8)))
  (let* ((pid-string (princ-to-string (pid portrait)))
         (portraits-dir (merge-pathnames "portraits/" dl-path))
         (sname (shortname (furre portrait)))
         (portrait-dir (merge-pathnames (uiop:strcat sname "/") portraits-dir))
         (data-path (merge-pathnames (uiop:strcat pid-string ".dat")
                                     portrait-dir)))
    (ensure-directories-exist portrait-dir)
    (write-byte-vector-into-file data data-path :if-exists :supersede)
    data))
