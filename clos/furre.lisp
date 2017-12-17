;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; furre.lisp

(in-package #:cl-furcadia/clos)

(defclass furre ()
  ((%name :accessor name
          :initarg :name
          :initform "")
   (%uid :accessor uid
         :initarg :uid
         :initform -1)
   (%last-login :accessor last-login
                :initarg :last-login
                :initform 0)
   (%description :accessor description
                 :initarg :description
                 :initform "")
   (%color-code :accessor color-code
                :initarg :color-code
                :initform "")
   (%digo :accessor digo
          :initarg :digo
          :initform 0)
   (%wings :accessor wings
           :initarg :wings
           :initform 0)
   (%portrait :accessor portrait
              :initarg :portrait
              :initform 0)
   (%tag :accessor tag
         :initarg :tag
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
                    :initform "")
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
                  :initform 0)
   (%digos :accessor digos
           :initarg :digos
           :initform '())
   (%lifers :accessor lifers
            :initarg :lifers
            :initform '())
   (%portraits :accessor portraits
               :initarg :portraits
               :initform '())
   (%specitags :accessor specitags
               :initarg :specitags
               :initform '())
   (%specitag-remap :accessor specitag-remap
                    :initarg :specitag-remap
                    :initform "")
   (%costumes :accessor costumes
              :initarg :costumes
              :initform '())))

(defmethod shortname ((furre furre))
  (name-shortname (name furre)))

(define-readable-print (furre stream :identity nil)
  (format stream "~S" (name furre)))
