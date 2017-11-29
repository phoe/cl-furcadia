;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; news.lisp

(defpackage #:cl-furcadia/news
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:drakma
   #:split-sequence))

(in-package #:cl-furcadia/news)

;;; HTTP news retrieval

(defvar *news-sources* '()
  "An alist of all news sources, where keys are keywords and values are URLs.")

(defun http-get-news (url)
  "Retrieves the news from the provided URL and returns it as a string."
  (let ((response (http-request url :external-format-out :utf-8)))
    (etypecase response
      (string response)
      ((vector (unsigned-byte 8)) (flex:octets-to-string response)))))

;; TODO log information and errors
(defun http-get-image (url pathname)
  "Retrieves the image from the provided URL and saves it under the following
pathname."
  (with-output-to-binary (output pathname :if-does-not-exist nil)
    (when output
      (finalized-let* ((input (drakma:http-request url :want-stream t)
                              (close input)))
        (copy-stream input output)))
    pathname))

;;; News preparation

(defclass news ()
  ((title :accessor title
          :initarg :title)
   (contents :accessor contents
             :initarg :contents)
   (category :accessor category
             :initarg :category)
   (date :accessor date
         :initarg :date)
   (datestring :accessor datestring
               :initarg :datestring)
   (url :accessor url
        :initarg :url)
   (image-path :accessor image-path
               :initarg :image-path)
   (image-filename :accessor image-filename
                   :initarg :image-filename))
  (:documentation "A piece of news."))

(define-constructor (news from)
  (when from
    (assert (listp from))
    (assert (= 8 (length from)))
    (destructuring-bind (title contents category date datestring
                         url image-path image-filename)
        from
      (setf (title news) title
            (contents news) contents
            (category news) category
            (date news) date
            (datestring news) datestring
            (url news) url
            (image-path news) image-path
            (image-filename news) image-filename))))

;; TODO rewrite as (make-instance 'news :from ...)
(defun make-news (title contents category date datestring
                  url image-path image-filename)
  "Makes a news instance from the provided information."
  (make-instance 'news :title title :contents contents :category category
                       :date date :datestring datestring :url url
                       :image-path image-path :image-filename image-filename))

(defun prepare-news (string)
  "Provided a string containing the retrieved news, returns a list of news
objects, sorted from newest."
  (let* ((news-data (split-sequence #\Newline string :remove-empty-subseqs t))
         (date (nth 7 news-data))
         (news (nthcdr 8 news-data)))
    (values news date)))

(defun split-csv-line (line)
  "Splits a news line into a list of strings."
  (let ((fare-csv:*separator* #\Tab))
    (fare-csv:read-csv-line (make-string-input-stream line))))
