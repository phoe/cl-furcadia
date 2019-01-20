;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; ws.lisp

(in-package :cl-furcadia/ws)

;;; Fetch login page

(defvar *url-login-get*
  "https://cms.furcadia.com/login")

(defun http-get-login-secret (&optional cookie-jar)
  (let ((cookie-jar (or cookie-jar (make-instance 'drakma:cookie-jar))))
    (let* ((page (drakma:http-request *url-login-get*
                                      :cookie-jar cookie-jar))
           (start (search "<input type=\"hidden\" name=\"return\"" page))
           (cut-page (subseq page (1+ start)))
           (start2 (search "<input" cut-page))
           (result (subseq cut-page (+ start2 27) (+ start2 27 32))))
      (assert (hexadecimal-string-p result))
      result)))

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

(defun login (email password)
  "Performs a full login with the provided email and password, returning the
cookie jar with associated login cookies."
  (let* ((cookie-jar (make-instance 'drakma:cookie-jar))
         (login-secret (http-get-login-secret cookie-jar)))
    (when (http-post-login email password login-secret cookie-jar)
      cookie-jar)))

;;; Fetch account

(defvar *url-fured-page*
  "https://cms.furcadia.com/fured/")

(defun http-get-account (cookie-jar)
  (let* ((page (drakma:http-request *url-fured-page*
                                    :cookie-jar cookie-jar))
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
    (values (make-instance 'standard-account :email email :id id :main main
                                             :gd gd :session session)
            (account-snames account-json)
            (account-last-logins account-json))))

(defun fetch-account (cookie-jar)
  "Fetches the account associated with the provided cookie jar from the Furcadia
web services."
  (json-account (http-get-account cookie-jar)))

;; Load costume

(defvar *url-fured-costume*
  "https://cms.furcadia.com/fured/loadCostume.php?cid=~D")

(defun http-get-costume (cid cookie-jar)
  (let* ((url (format nil *url-fured-costume* cid))
         (response (drakma:http-request url :cookie-jar cookie-jar)))
    (decode-json (make-string-input-stream response))))

(defparameter *json-costume-ignored-keywords*
  '(:state :glom))

(defparameter *json-costume-keywords*
  `((:cid cid parse-integer)
    (:name name)
    (:ord ordinal parse-integer)
    (:desc description)
    (:colr color-code)
    (:digo digo)
    (:port portrait parse-integer)
    (:scal scale parse-integer)
    ;; (:glom glom) ;; TODO figure out what it is
    (:tag specitag parse-integer)
    (:strd rating ,(lambda (x) (assoc-value *desc-standards*
                                            (parse-integer x))))
    (:aresp auto-response)
    (:atime afk-time parse-integer)
    (:amaxtime afk-max-time parse-integer)
    (:doresp auto-response-p ,(lambda (x) (/= (parse-integer x) 0)))
    (:adigo afk-digo)
    (:awhsp afk-whisper)
    (:aport afk-portrait parse-integer)
    (:wing wings)
    (:awing afk-wings)
    (:adesc afk-description)
    (:acolr afk-color-code)))

(defun json-costume (json &optional furre)
  (loop with instance = (make-instance 'standard-costume)
        for (keyword . value) in json
        for entry = (assoc keyword *json-costume-keywords*)
        if (and (null entry)
                (not (member keyword *json-costume-ignored-keywords*)))
          collect (cons keyword value) into unknowns
        else unless (member keyword *json-costume-ignored-keywords*) do
          (destructuring-bind (keyword accessor . maybe-fn) entry
            (declare (ignore keyword))
            (let* ((filter (or (car maybe-fn) #'identity))
                   (setter (fdefinition (list 'setf accessor))))
              (funcall setter (funcall filter value) instance)))
        finally (setf (furre instance) furre)
                (return (values instance unknowns))))

(defun fetch-costume (cid cookie-jar)
  (json-costume (http-get-costume cid cookie-jar)))

;; Load portrait

(defvar *url-fured-portrait*
  "https://cms.furcadia.com/fured/loadPortrait.php?pid=~D")

(defun fetch-portrait (pid cookie-jar)
  (let* ((url (format nil *url-fured-portrait* pid)))
    (multiple-value-bind (stream status headers uri stream2 closedp reason)
        (drakma:http-request url :cookie-jar cookie-jar :want-stream t)
      (declare (ignore headers uri stream2 closedp))
      (unless (= 2 (truncate status 100))
        (error "HTTP request unsuccessful (~D): ~A" status reason))
      (let* ((stream (flex:flexi-stream-stream stream))
             (type (ecase (read-byte stream)
                     ((0 1) :8-bit) (2 :24-bit) (3 :fox)))
             (remappedp (ecase (read-byte stream) (0 nil) (1 t)))
             (data (read-stream-content-into-byte-vector stream)))
        (make-instance 'standard-portrait :portrait-type type :pid pid
                                          :remappedp remappedp :data data)))))

;; Load specitag

(defvar *url-apollo-specitag*
  "http://apollo.furcadia.com/cache/~D.png")

(defun fetch-specitag (sid)
  (let ((url (format nil *url-apollo-specitag* sid)))
    (multiple-value-bind (stream status headers uri stream2 closedp reason)
        (drakma:http-request url :want-stream t :force-binary t)
      (declare (ignore headers uri stream2 closedp))
      (unless (= 2 (truncate status 100))
        (error "HTTP request unsuccessful (~D): ~A" status reason))
      (let* ((data (pngload:load-stream stream :flatten t))
             (remappedp (ecase (truncate sid 10000000) (50 nil) (59 t))))
        (make-instance 'standard-specitag :index sid :data (pngload:data data)
                                          :remappedp remappedp)))))

;; Load image

(defvar *url-raptor-systems-images*
  "https://raptor.systems/furcadia/images/~A")

(defun fetch-image-list (shortname)
  (let ((url (format nil *url-raptor-systems-images* shortname))
        (image-keys '(:id :timestamp :url :eye-level :sfw)))
    (multiple-value-bind (stream status headers uri stream2 closedp reason)
        (drakma:http-request url :want-stream t)
      (declare (ignore headers uri stream2 closedp))
      (cond
        ((= status 404) '())
        ((/= 2 (truncate status 100))
         (error "HTTP request unsuccessful (~D): ~A" status reason))
        (t (let* ((json (cl-json:decode-json stream)))
             (loop for entry in json
                   do (assert (set-equal image-keys (mapcar #'car entry)))
                   collect (apply #'make-instance 'standard-image
                                  (alist-plist entry)))))))))

;;; Fetch furre

(defvar *url-fured-load*
  "https://cms.furcadia.com/fured/loadCharacter.php")

(defun http-load-furre (sname cookie-jar)
  (let ((page (drakma:http-request *url-fured-load*
                                   :method :post
                                   :parameters `(("name" . ,sname))
                                   :cookie-jar cookie-jar)))
    (decode-json (make-string-input-stream page))))

(defun parse-costume-forms (forms)
  (mapcar (lambda (x) (list (parse-integer (symbol-name (car x)))
                            (parse-integer (cadr x))
                            (caddr x)))
          forms))

(defun parse-specitag-forms (forms)
  (mapcar (lambda (x) (list (parse-integer (symbol-name (car x)))
                            (cdr x)))
          forms))

(defparameter *json-furre-ignored-keywords*
  '(:snam :state :specitags))

(defparameter *json-furre-keywords*
  `((:name name ,(lambda (x) (substitute #\Space #\| x)))
    (:uid uid parse-integer)
    (:digos digos)
    (:lifers lifers)
    (:ports portraits)
    (:specitag-remap specitags parse-specitag-forms)
    (:costumes costumes parse-costume-forms)))

(defun json-furre (json)
  (loop with furre = (make-instance 'standard-furre)
        with costume = (make-instance 'standard-costume)
        for (keyword . value) in json
        for furre-entry = (assoc keyword *json-furre-keywords*)
        for costume-entry = (assoc keyword *json-costume-keywords*)
        if (and (null furre-entry) (null costume-entry)
                (not (member keyword *json-furre-ignored-keywords*))
                (not (member keyword *json-costume-ignored-keywords*)))
          collect (cons keyword value) into unknowns
        else
          if (and furre-entry
                  (not (member keyword *json-furre-ignored-keywords*)))
            do (destructuring-bind (keyword accessor . maybe-fn) furre-entry
                 (declare (ignore keyword))
                 (let* ((filter (or (car maybe-fn) #'identity))
                        (setter (fdefinition (list 'setf accessor))))
                   (funcall setter (funcall filter value) furre)))
        else
          if (and costume-entry
                  (not (member keyword *json-costume-ignored-keywords*)))
            do (destructuring-bind (keyword accessor . maybe-fn) costume-entry
                 (declare (ignore keyword))
                 (let* ((filter (or (car maybe-fn) #'identity))
                        (setter (fdefinition (list 'setf accessor))))
                   (funcall setter (funcall filter value) costume)))
        finally (push costume (costumes furre))
                (setf (furre costume) furre)
                (return (values furre unknowns))))

(defun fetch-furre (sname cookie-jar)
  "Fetches the furre with the provided shortname from the Furcadia web services,
using the provided cookie jar."
  (json-furre (http-load-furre sname cookie-jar)))

;;; Save furre to Furcadia WS

(defvar *url-fured-save*
  "https://cms.furcadia.com/fured/saveCharacter.php")

(defun furre-json (furre account)
  (let* ((session (session account))
         (furre-data
           (loop for (keyword fn) in *json-furre-keywords*
                 for keystring = (string-downcase (princ-to-string keyword))
                 for value = (princ-to-string (or (funcall fn furre) ""))
                 unless (string= value "")
                   collect (cons keystring value))))
    (nconc furre-data (list (cons "tokenRequest" "true")
                            (cons "tokenCostume" "-1")
                            (cons session "1")))))

(defun http-save-furre (furre account cookie-jar)
  (let* ((json (furre-json furre account))
         (response (drakma:http-request *url-fured-save*
                                        :method :post
                                        :parameters json
                                        :cookie-jar cookie-jar))
         (result (decode-json (make-string-input-stream response))))
    (values (assoc-value result :login--url)
            result)))

(defun save-furre (furre account cookie-jar)
  "Saves the provided furre to Furcadia web services, using the provided account
and cookie jar. Returns the Furcadia login URI, or NIL if the save was
unsuccessful."
  (values (http-save-furre furre account cookie-jar)))

;;; TODO likely doesn't work anymore
;; ;;; SLOW, SERIAL IMPLEMENTATION - FOR REPRESENTATION ONLY
;; (defun fetch-everything (cookie-jar)
;;   (multiple-value-bind (account snames last-logins) (fetch-account cookie-jar)
;;     (prog1 account
;;       (let ((furres (mapcar (rcurry #'fetch-furre cookie-jar) snames)))
;;         (setf (furres account) furres)
;;         (loop for furre in furres
;;               for last-login in last-logins
;;               do (setf (last-login furre) last-login)
;;                  (loop for costume-list on (costumes furre)
;;                        for costume = (car costume-list)
;;                        if (listp costume)
;;                          do (let ((json (http-get-costume (second costume)
;;                                                           cookie-jar)))
;;                               (setf (car costume-list)
;;                                     (json-costume json furre)))))))))

;;; TODO check what happens if tokenCostume = 0 and/or tokenRequest = false
