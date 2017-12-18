;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; ws.lisp

(in-package :cl-furcadia/ws)

;;; Config
;;; TODO create config protocol

(defun make-config ()
  (let ((config (make-hash-table)))
    (prog1 config
      (setf (cookie-jar config) (make-instance 'drakma:cookie-jar)))))

(defun cookie-jar (config)
  (gethash :cookie-jar config))

(defun (setf cookie-jar) (new-value config)
  (setf (gethash :cookie-jar config) new-value))

;;; Fetch login page

(defvar *url-login-get*
  "https://cms.furcadia.com/login")

(defun http-get-login-secret (&optional (config (make-config)))
  (let* ((page (drakma:http-request *url-login-get*
                                    :cookie-jar (cookie-jar config)))
         (start (search "<input type=\"hidden\" name=\"return\"" page))
         (cut-page (subseq page (1+ start)))
         (start2 (search "<input" cut-page))
         (result (subseq cut-page (+ start2 27) (+ start2 27 32))))
    (assert (hexadecimal-string-p result))
    result))

;;; Do login

(defvar *url-login-post*
  "https://cms.furcadia.com/index.php?option=com_sphinx&task=user.login")

(defun http-post-login (email password login-secret config)
  (flet ((parameters (email password login-secret)
           `(("username" . ,email)
             ("password" . ,password)
             (,login-secret . "1"))))
    (let* ((parameters (parameters email password login-secret))
           (page (drakma:http-request *url-login-post*
                                      :method :post
                                      :parameters parameters
                                      :cookie-jar (cookie-jar config))))
      (when (search "Logout" page) page))))

;; TODO export
(defun login (email password)
  "Performs a full login with the provided email and password, returning the
cookie jar with associated login cookies."
  (let* ((config (make-config))
         (login-secret (http-get-login-secret config)))
    (when (http-post-login email password login-secret config)
      config)))

;;; Fetch account

(defvar *url-fured-page*
  "https://cms.furcadia.com/fured/")

;;; TODO export
(defun http-get-account (config)
  (let* ((page (drakma:http-request *url-fured-page*
                                    :cookie-jar (cookie-jar config)))
         (begin (search "account.JSON=" page))
         (end (search (string #\Newline) page :start2 begin))
         (subseq (subseq page (+ begin 13) (1- end)))
         (json (decode-json (make-string-input-stream subseq))))
    json))

(defun account-snames (account-json)
  (mapcar (lambda (x) (assoc-value x :shortname))
          (assoc-value account-json :characters)))

(defun account-last-logins (account-json)
  (mapcar (lambda (x) (assoc-value x :login-date))
          (assoc-value account-json :characters)))

(defun json-account (account-json)
  (let* ((email (assoc-value account-json :email))
         (id (parse-integer (assoc-value account-json :aid)))
         (main (assoc-value account-json :main))
         (gd (assoc-value account-json :gd))
         (session (assoc-value account-json :session)))
    (values (make-instance 'account :email email :id id :main main
                                    :gd gd :session session)
            (account-snames account-json)
            (account-last-logins account-json))))

;;; TODO export
(defun fetch-account (config)
  (json-account (http-get-account config)))

;;; Fetch furre

(defvar *url-fured-load*
  "https://cms.furcadia.com/fured/loadCharacter.php")

(defun http-load-furre (sname config)
  (let ((page (drakma:http-request *url-fured-load*
                                   :method :post
                                   :parameters `(("name" . ,sname))
                                   :cookie-jar (cookie-jar config))))
    (decode-json (make-string-input-stream page))))

(defparameter *furre-json-keywords*
  `((:name name ,(lambda (x) (substitute #\Space #\| x)))
    (:uid uid parse-integer)
    (:desc description)
    (:colr color-code)
    (:digo digo)
    (:wing wings)
    (:port portrait parse-integer)
    (:tag tag parse-integer)
    (:aresp auto-response)
    (:doresp auto-response-p ,(lambda (x) (/= (parse-integer x) 0)))
    (:adesc afk-description)
    (:awhsp afk-whisper)
    (:acolr afk-color-code)
    (:adigo afk-digo)
    (:awing afk-wings)
    (:aport afk-portrait parse-integer)
    (:atime afk-time parse-integer)
    (:amaxtime afk-max-time parse-integer)
    (:digos digos)
    (:lifers lifers)
    (:ports portraits)
    (:specitags specitags)
    (:specitag-remap specitag-remap)
    (:costumes costumes)))

(defparameter *json-furre-ignored-keywords*
  '(:snam :state))

(defun json-furre (json)
  (loop with instance = (make-instance 'furre)
        for (keyword . value) in json
        for entry = (assoc keyword *furre-json-keywords*)
        if (null entry)
          collect (cons keyword value) into unknowns
        else unless (member keyword *json-furre-ignored-keywords*) do
          (destructuring-bind (keyword accessor . maybe-fn) entry
            (declare (ignore keyword))
            (let* ((fn (or (car maybe-fn) #'identity))
                   (setf (fdefinition (list 'setf accessor))))
              (funcall setf (funcall fn value) instance)))
        finally (return (values instance unknowns))))

;;; TODO export
(defun fetch-furre (sname config)
  (json-furre (http-load-furre sname config)))

;;; SLOW, SERIAL IMPLEMENTATION - FOR REPRESENTATION ONLY
(defun fetch-everything (config)
  (multiple-value-bind (account snames last-logins) (fetch-account config)
    (let ((furres (mapcar (rcurry #'fetch-furre config) snames)))
      (setf (furres account) furres)
      (loop for furre in furres
            for last-login in last-logins
            do (setf (last-login furre) last-login)))
    account))

(defun furre-json (furre account)
  (let* ((session (session account))
         (furre-data
           (loop for (keyword fn) in *furre-json-keywords*
                 for keystring = (string-downcase (princ-to-string keyword))
                 for value = (princ-to-string (or (funcall fn furre) ""))
                 unless (string= value "")
                   collect (cons keystring value))))
    (nconc furre-data (list (cons "tokenRequest" "true")
                            (cons "tokenCostume" "-1")
                            (cons session "1")))))

(defvar *url-fured-save*
  "https://cms.furcadia.com/fured/saveCharacter.php")

;;; TODO export
(defun http-save-furre (furre account config)
  (let* ((json (furre-json furre account))
         (response (drakma:http-request *url-fured-save*
                                        :method :post
                                        :parameters json
                                        :cookie-jar (cookie-jar config)))
         (result (decode-json (make-string-input-stream response))))
    (values (assoc-value result :login--url)
            result)))
