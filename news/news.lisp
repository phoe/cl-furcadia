;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; news.lisp

(defpackage #:cl-furcadia/news
  (:use
   #:cl
   #:alexandria
   #:phoe-toolbox
   #:fare-csv
   #:drakma
   #:split-sequence)
  (:import-from #:local-time #:encode-timestamp #:timestamp>)
  (:import-from #:trivial-download #:download)
  (:export
   ;; Functions
   #:*news-sources*
   #:*example-news-sources*
   #:get-all-news
   #:http-download
   #:http-download-all
   ;; News class
   #:news
   #:title
   #:contents
   #:category
   #:date
   #:datestring
   #:url
   #:image-url
   #:image-filename
   ))

(in-package #:cl-furcadia/news)

;;; HTTP news retrieval

(defvar *news-sources* '()
  "An alist of all news sources, where keys are keywords and values are URLs.")

(defvar *example-news-sources*
  '("http://news.furcadia.com/current" ;; Furcadia
    "http://raptorlauncher.github.io/news.txt" ;; Raptor Launcher
    )
  "Example news sources known to be usable at the time of writing this code.")

(defun http-get-news (url)
  "Retrieves the news from the provided URL and returns it as a string."
  (let ((response (http-request url :external-format-out :utf-8)))
    (etypecase response
      (string response)
      ((vector (unsigned-byte 8)) (flex:octets-to-string response)))))

(defun get-all-news (urls &optional last-modified)
  "Fetches all news from the provided URLs and returns them sorted from newest
to oldest. The second value returns the newest date fetched from the news
sources and, if supplied, the LAST-MODIFIED argument."
  (check-type last-modified (or null string))
  (multiple-value-bind (news dates)
      (multiple-value-mapcar (compose #'prepare-news #'http-get-news) urls)
    (when last-modified
      (setf dates (cons last-modified dates)))
    (values (sort (apply #'nconc news) #'timestamp> :key #'date)
            (extremum dates #'string>))))

(defun http-download (url pathname)
  ;; TODO logging
  (download url pathname :quiet t))

(defun http-download-all (urls directory)
  (let* ((filenames (mapcar #'url-filename urls))
         (pathnames (mapcar (rcurry #'merge-pathnames directory) filenames)))
    (mapc (rcurry #'http-download) urls pathnames)))

;;; News class

;; TODO protocolize this
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

;;; News preparation

(defun prepare-news (string)
  "Provided a string containing the retrieved news, returns a list of news
objects, sorted from newest."
  (let* ((news-data (split-sequence #\Newline string :remove-empty-subseqs t))
         (date (subseq (nth 7 news-data) 9))
         (news (nthcdr 8 news-data))
         (cut-news (mapcar #'prepare-csv-line news)))
    (values cut-news date)))

(defun prepare-csv-line (line)
  "Converts a news line into an instance of NEWS object."
  (let* ((fare-csv:*separator* #\Tab)
         (splits (fare-csv:read-csv-line (make-string-input-stream line))))
    (setf (first splits) (subseq (first splits) 10)
          (cdr splits) (cddr splits))
    (destructuring-bind (datestring category title contents url image-url)
        splits
      (let ((parsed-date (cl-furcadia/date-parser:parse-date datestring)))
        (destructuring-bind (day month year) parsed-date
          (let* ((timestamp (encode-timestamp 0 0 0 0 day month year))
                 (image-filename (url-filename image-url))
                 (from (list title contents category timestamp datestring
                             url image-url image-filename)))
            (make-instance 'news :from from)))))))

(defun url-filename (url)
  "Given a URL, returns everything after its last slash."
  (assert (stringp url))
  (let* ((position (position #\/ url :from-end t)))
    (if position
        (subseq url (1+ position))
        "")))
