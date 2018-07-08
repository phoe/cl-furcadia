;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; standard-costume.lisp

(in-package #:cl-furcadia/clos)

(defclass standard-costume (costume)
  ((%cid :accessor cid
         :initarg :cid
         :initform 0)
   (%name :accessor name
          :initarg :name
          :initform "Default Appearance")
   (%rating :accessor rating
            :initarg :rating
            :initform :t+)
   (%furre :accessor furre
           :initarg :furre
           :initform nil)
   (%scale :accessor scale
           :initarg :scale
           :initform 100)
   (%ordinal :accessor ordinal
             :initarg :ordinal
             :initform 0)
   (%description :accessor description
                 :initarg :description
                 :initform "")
   (%color-code :accessor color-code
                :initarg :color-code
                :initform "w###############")
   (%digo :accessor digo
          :initarg :digo
          :initform 0)
   (%specitag :accessor specitag
              :initarg :specitag
              :initform 0)
   (%wings :accessor wings
           :initarg :wings
           :initform 0)
   (%portrait :accessor portrait
              :initarg :portrait
              :initform 0)
   (%auto-response :accessor auto-response
                   :initarg :auto-response
                   :initform "")
   (%auto-response-p :accessor auto-response-p
                     :initarg :auto-response-p
                     :initform nil)
   (%afk-description :accessor afk-description
                     :initarg :afk-description
                     :initform "")
   (%afk-whisper :accessor afk-whisper
                 :initarg :afk-whisper
                 :initform "")
   (%afk-color-code :accessor afk-color-code
                    :initarg :afk-color-code
                    :initform "w###############")
   (%afk-digo :accessor afk-digo
              :initarg :afk-digo
              :initform 0)
   (%afk-wings :accessor afk-wings
               :initarg :afk-wings
               :initform 0)
   (%afk-portrait :accessor afk-portrait
                  :initarg :afk-portrait
                  :initform 0)
   (%afk-time :accessor afk-time
              :initarg :afk-time
              :initform 0)
   (%afk-max-time :accessor afk-max-time
                  :initarg :afk-max-time
                  :initform 0)))

(define-readable-print (standard-costume stream :identity nil)
  (format stream "~S" (name standard-costume))
  (when-let ((furre (furre standard-costume)))
    (format stream " (~S)" (name furre))))
