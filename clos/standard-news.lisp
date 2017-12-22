;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; news.lisp

(in-package :cl-furcadia/clos)

(defclass standard-news (news)
  ((%title :accessor title
           :initarg :title)
   (%contents :accessor contents
              :initarg :contents)
   (%category :accessor category
              :initarg :category)
   (%date :accessor date
          :initarg :date)
   (%datestring :accessor datestring
                :initarg :datestring)
   (%url :accessor url
         :initarg :url)
   (%image-url :accessor image-url
               :initarg :image-url)
   (%image-filename :accessor image-filename
                    :initarg :image-filename))
  (:documentation "A piece of news."))

(define-constructor (standard-news from)
  (when from
    (assert (listp from))
    (assert (= 8 (length from)))
    (destructuring-bind (title contents category date datestring
                         url image-url image-filename)
        from
      (setf (title standard-news) title
            (contents standard-news) contents
            (category standard-news) category
            (date standard-news) date
            (datestring standard-news) datestring
            (url standard-news) url
            (image-url standard-news) image-url
            (image-filename standard-news) image-filename))))

(define-readable-print (standard-news stream :identity nil)
  (format stream "[~A] ~A (~A)"
          (category standard-news) (title standard-news)
          (datestring standard-news)))
