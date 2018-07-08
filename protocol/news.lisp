;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; news.lisp

(in-package :cl-furcadia/protocol)

(define-protocol news
    (:documentation "The NEWS protocol describes objects representing news for ~
the MMOSG Furcadia. These news have basic textual information, such as a title ~
and contents, contain date and category information and may contain images and
hyperlinks."
     :tags (:cl-furcadia :news)
     :export t)
  (:class news () ())
  "A news object. Each class participating in the protocol must subclass this ~
protocol class."
  (:function title ((news news)) string)
  "Returns the title of the news."
  (:function (setf title) ((new-value string) (news news)) string)
  "Sets the title of the news."
  (:function contents ((news news)) string)
  "Returns the contents of the news."
  (:function (setf contents) ((new-value string) (news news)) string)
  "Sets the contents of the news."
  (:function category ((news news)) string)
  "Returns the category of the news."
  (:function (setf category) ((new-value string) (news news)) string)
  "Sets the category of the news."
  ;; TODO type T? or date protocol? lol, I can import GATEWAY/PROTOCOL:DATE
  (:function date ((news news)) t)
  "Returns the date of the news."
  (:function (setf date) ((new-value t) (news news)) t)
  "Returns the date of the news."
  (:function url ((news news)) (or string null))
  "Returns the URL of the news, or NIL if there is none."
  (:function (setf url)
             ((new-value (or string null)) (news news)) (or string null))
  "Returns the URL of the news, or NIL if there is none."
  (:function image-url ((news news)) (or string null))
  "Returns the URL of the news's image, or NIL if there is none."
  (:function (setf image-url)
             ((new-value (or string null)) (news news)) (or string null))
  "Returns the URL of the news's image, or NIL if there is none.")

(execute-protocol news)
