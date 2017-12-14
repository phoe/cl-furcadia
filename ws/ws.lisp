;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; characters.lisp

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

(defun http-login-page (&optional (config (make-config)))
  (let ((page (drakma:http-request *url-login-get*
                                   :cookie-jar (cookie-jar config))))
    (values page config)))

(defun extract-login-page-secret (page)
  (let* ((start (search "<input type=\"hidden\" name=\"return\"" page))
         (cut-page (subseq page (1+ start)))
         (start2 (search "<input" cut-page))
         (result (subseq cut-page (+ start2 27) (+ start2 27 32))))
    (assert (hexadecimal-string-p result))
    result))

;;; Do login

(defvar *url-login-post*
  "https://cms.furcadia.com/index.php?option=com_sphinx&task=user.login")

(defun http-post-login (email password login-secret cookie-jar)
  (flet ((parameters (email password login-secret)
           `(("username" . ,email)
             ("password" . ,password)
             (,login-secret . "1"))))
    (let* ((parameters (parameters email password login-secret))
           (page (drakma:http-request *url-login-post*
                                      :method :post
                                      :parameters parameters
                                      :cookie-jar cookie-jar)))
      (when (search "Logout" page) page))))

(defun do-login (email password)
  "Performs a full login with the provided email and password, returning the
cookie jar with associated login cookies."
  (let* ((config (make-config))
         (cookie-jar (cookie-jar config))
         (login-page (http-login-page config))
         (login-secret (extract-login-page-secret login-page)))
    (when (http-post-login email password login-secret cookie-jar)
      config)))
