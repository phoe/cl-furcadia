;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; news.lisp

(in-package :cl-furcadia/clos)

(defclass news ()
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

(define-constructor (news from)
  (when from
    (assert (listp from))
    (assert (= 8 (length from)))
    (destructuring-bind (title contents category date datestring
                         url image-url image-filename)
        from
      (setf (title news) title
            (contents news) contents
            (category news) category
            (date news) date
            (datestring news) datestring
            (url news) url
            (image-url news) image-url
            (image-filename news) image-filename))))

(define-readable-print (news stream :identity nil)
  (format stream "[~A] ~A (~A)"
          (category news) (title news) (datestring news)))
