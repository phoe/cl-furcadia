;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; news.lisp

(in-package #:cl-furcadia/news)

;;; HTTP news retrieval

(defvar *news-sources*
  '("http://news.furcadia.com/current"
    "http://raptorlauncher.github.io/news.txt")
  "An alist of all news sources, where keys are keywords and values are URLs.
The default value contains URLs known to be good at the time of writing this
library.
\
This variable is expected to be rebound by clients which use it.")

(defun get-news (&key (urls *news-sources*) last-modified (mapcar #'mapcar))
  "Fetches all news from the provided URLs and returns them sorted from newest
to oldest. The second value returns the newest date fetched from the news
sources and, if supplied, the LAST-MODIFIED keyword argument.
The function under the MAPCAR argument can be replaced by a parallel
implementation, such as PMAPCAR from the LPARALLEL package."
  (check-type last-modified (or null string))
  (check-type mapcar function)
  (let ((strings (funcall mapcar #'http-get-news urls))
        (news '())
        (dates '()))
    (dolist (string strings)
      (multiple-value-bind (new date) (prepare-news string)
        (appendf news new)
        (push date dates)))
    (when last-modified
      (push last-modified dates))
    (values (sort news #'timestamp> :key #'date)
            (extremum dates #'string>))))

(defun http-get-news (url)
  "Retrieves the news from the provided URL and returns it as a string."
  (let ((response (http-request url :external-format-out :utf-8)))
    (etypecase response
      (string response)
      ((vector (unsigned-byte 8)) (flex:octets-to-string response)))))

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

;; TODO implement this in Raptor Launcher instead, since it'll have both
;; drakma and lparallel in its dependencies
;; (defun http-download-all (urls directo3ry)
;;   (flet ((http-download (url pathname)
;;            ;; TODO logging here
;;            (download url pathname :quiet t)))
;;     (let* ((filenames (mapcar #'url-filename urls))
;;            (pathnames (mapcar (rcurry #'merge-pathnames directory) filenames)))
;;       (mapc (rcurry #'http-download) urls pathnames))))
