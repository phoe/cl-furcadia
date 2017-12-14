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

;;; Fetch furre

(defvar *url-fured-load*
  "https://cms.furcadia.com/fured/loadCharacter.php")

(defun http-load-furre (sname config)
  (let ((page (drakma:http-request *url-fured-load*
                                   :method :post
                                   :parameters `(("name" . ,sname))
                                   :cookie-jar (cookie-jar config))))
    (decode-json (make-string-input-stream page))))

;;; TODO create an account class

(defun json-account (account-json)
  (let* ((email (assoc-value account-json :email))
         (id (parse-integer (assoc-value account-json :aid)))
         (main (assoc-value account-json :main))
         (gd (assoc-value account-json :gd))
         (characters (assoc-value account-json :characters)))
    (values (make-instance 'account :email email :id id :main main :gd gd)
            characters)))

;;; TODO export after creating account class
(defun account-snames (account-json)
  "Given an account JSON"
  (mapcar (lambda (x) (cdr (assoc :shortname x)))
          (cdr (assoc :characters account-json))))
