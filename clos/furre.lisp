;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; furre.lisp

(in-package #:cl-furcadia/clos)

;; TODO compute all sane default values and their types and store them here
(defclass furre ()
  ((%name :accessor name
          :initarg :name)
   %(shortname :accessor shortname
               :initform :shortname) ;; TODO compute this based on name
   (%uid :accessor uid
         :initarg uid)
   (%description :accessor description
                 :initarg :description)
   (%color-code :accessor color-code
                :initarg :color-code)
   (%digo :accessor digo
          :initarg :digo)
   (%wings :accessor wings
           :initarg :wings)
   (%portrait :accessor portrait
              :initarg :portrait)
   (%tag :accessor tag
         :initarg :tag)
   (%whisper-response :accessor whisper-response
                      :initarg :whisper-response)
   (%afk-description :accessor afk-description
                     :initarg :afk-description)
   (%afk-whisper :accessor afk-whisper
                 :initarg :afk-whisper)
   (%afk-whisper-response :accessor afk-whisper-response
                          :initarg :afk-whisper-response)
   (%afk-color-code :accessor afk-color-code
                    :initarg :afk-color-code)
   (%afk-digo :accessor afk-digo
              :initarg :afk-digo)
   (%afk-wings :accessor afk-wings
               :initarg :afk-wings)
   (%afk-portrait :accessor afk-portrait
                  :initarg :afk-portrait)
   (%afk-time :accessor afk-time
              :initarg :afk-time)
   (%afk-max-time :accessor afk-max-time
                  :initarg :afk-max-time)
   (%digos :accessor digos
           :initarg :digos)
   (%lifers :accessor lifers
            :initarg :lifers)
   (%portraits :accessor portraits
               :initarg :portraits)
   (%specitags :accessor specitags
               :initarg :specitags)
   (%specitag-remap :accessor specitag-remap
                    :initarg :specitag-remap)
   (%costumes :accessor costumes
              :initarg :costumes)))

(defmacro check-boundp (object accessor)
  (let ((slot-name (symbolicate #\% accessor)))
    `(unless (slot-boundp ,object ',slot-name)
       (error "Must provide ~A." ',accessor))))

(define-constructor (furre)
  (check-boundp furre name))

(define-print (furre stream)
  (format stream "~S" (name furre)))
