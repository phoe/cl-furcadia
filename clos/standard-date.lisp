;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-date.lisp

(in-package :cl-furcadia/clos)

;; TODO unbreak tests
(defclass standard-date (date local-time:timestamp) ()
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class DATE.")))

(defmethod date-timestamp ((date standard-date))
  (local-time:timestamp-to-unix date))

(defmethod timestamp-date-using-class
    ((class (eql (find-class 'standard-date)))
     (timestamp integer))
  (change-class (local-time:unix-to-timestamp timestamp) 'standard-date))

(defmethod date-ustimestamp ((date standard-date))
  (let ((timestamp (date-timestamp date))
        (nsec (truncate (local-time:nsec-of date) 1000)))
    (+ (* 1000000 timestamp) nsec)))

(defmethod ustimestamp-date-using-class
    ((class (eql (find-class 'standard-date)))
     (nstimestamp integer))
  (multiple-value-bind (timestamp msec) (truncate nstimestamp 1000000)
    (let ((date (timestamp-date-using-class class timestamp)))
      (setf (local-time:nsec-of date) (* 1000 msec))
      date)))

(defmethod date= ((date-1 standard-date) (date-2 standard-date)
                  &key (unit :microsecond))
  (assert (member unit *date-granularity-units*))
  (if (eq unit :microsecond)
      (local-time:timestamp= date-1 date-2)
      (every #'= (date-elts date-1 unit) (date-elts date-2 unit))))

(defun date-elts (date unit)
  (subseq (nreverse (date=-decode date)) 4
          (+ 5 (position unit *date-granularity-units*))))

(defun date=-decode (date)
  (multiple-value-list
   (local-time:decode-timestamp date :timezone local-time:+utc-zone+)))

(defmethod date/= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :microsecond))
  (not (date= date-1 date-2 :unit unit)))

(defmethod date<= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :microsecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp< date-1 date-2)))

(defmethod date>= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :microsecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp> date-1 date-2)))

(defmethod date> ((date-1 standard-date) (date-2 standard-date)
                  &key (unit :microsecond))
  (not (date<= date-1 date-2 :unit unit)))

(defmethod date< ((date-1 standard-date) (date-2 standard-date)
                  &key (unit :microsecond))
  (not (date>= date-1 date-2 :unit unit)))

(defmethod date-min (date &rest other-dates)
  (apply #'local-time:timestamp-minimum date other-dates))

(defmethod date-max (date &rest other-dates)
  (apply #'local-time:timestamp-maximum date other-dates))

(defmethod now-using-class ((class (eql (find-class 'standard-date))))
  (change-class (local-time:now) 'standard-date))

(defun now () (now-using-class (find-class 'standard-date)))

;;; TESTS

;; TODO separate test package
#|
(define-test-case standard-date-unit
    (:description "Unit tests for STANDARD-DATE."
     :tags (:unit :date)
     :type :unit-suite))

(define-test standard-date-unit
    (flet ((mkdt (d s ns) (make-instance 'standard-date :day d :sec s :nsec ns)))
      (let* ((d-orig (mkdt 0 0 0)) (d-same (mkdt 0 0 0)) (d-usec (mkdt 0 0 1000))
             (d-sec (mkdt 0 1 0)) (d-min (mkdt 0 60 0)) (d-hour (mkdt 0 3600 0))
             (d-day (mkdt 1 0 0)) (d-month (mkdt 31 0 0)) (d-year (mkdt 365 0 0))
             (vars (list d-orig d-usec d-sec d-min d-hour d-day d-year)))
        (is (listp (serialize d-orig :type :list)))
        (is (date= d-orig (deserialize-using-class
                           (find-class 'standard-date)
                           (serialize d-orig :type :list))))
        (is (stringp (serialize d-orig :type :string)))
        (is (date= d-orig (deserialize-using-class
                           (find-class 'standard-date)
                           (read-from-string (serialize d-orig :type :string)))))
        (is (integerp (date-timestamp d-orig)))
        (is (date= d-orig (timestamp-date-using-class (find-class 'standard-date)
                                                      (date-timestamp d-orig))))
        (is (integerp (date-ustimestamp d-orig)))
        (is (date= d-orig
                   (ustimestamp-date-using-class (find-class 'standard-date)
                                                 (date-ustimestamp d-orig))))
        (is (date= d-orig d-same))
        (is (date= d-orig d-same :unit :microsecond))
        (is (date= d-orig d-usec :unit :second))
        (is (date= d-orig d-sec :unit :minute))
        (is (date= d-orig d-min :unit :hour))
        (is (date= d-orig d-hour :unit :day))
        (is (date= d-orig d-day :unit :month))
        (is (date= d-orig d-month :unit :year))
        (is (date/= d-orig d-usec))
        (is (date/= d-orig d-sec))
        (is (date/= d-orig d-min))
        (is (date/= d-orig d-hour))
        (is (date/= d-orig d-day))
        (is (date/= d-orig d-month))
        (is (date/= d-orig d-year))
        (is (date/= d-orig d-sec :unit :microsecond))
        (is (date/= d-orig d-min :unit :second))
        (is (date/= d-orig d-hour :unit :minute))
        (is (date/= d-orig d-day :unit :hour))
        (is (date/= d-orig d-month :unit :day))
        (is (date/= d-orig d-year :unit :month))
        (is (date< d-orig d-usec))
        (is (date< d-usec d-sec))
        (is (date< d-sec d-min))
        (is (date< d-min d-hour))
        (is (date< d-hour d-day))
        (is (date< d-day d-month))
        (is (date< d-month d-year))
        (is (date> d-usec d-orig))
        (is (date> d-sec d-usec))
        (is (date> d-min d-sec))
        (is (date> d-hour d-min))
        (is (date> d-day d-hour))
        (is (date> d-month d-day))
        (is (date> d-year d-month))
        (is (eq d-orig (apply #'date-min vars)))
        (is (eq d-year (apply #'date-max vars)))
        (is (typep (now-using-class (find-class 'standard-date)) 'standard-date))
        )))
|#
