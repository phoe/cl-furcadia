;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; post.lisp

(in-package :cl-furcadia/protocol)

(define-protocol post
    (:documentation "The POST protocol describes objects representing textual ~
posts within the MMOSG Furcadia. Each post has an author being the furre who ~
authored and sent that post, a date marking when the post was authored, and ~
post contents which are a single line of text." ;; TODO text limit in Furc
     ;; TODO max 4080 chars per whisper
     :tags (:cl-furcadia :post)
     :export t)
  (:class post () ())
  "A post object. Each class participating in this protocol must subclass this ~
protocol class."
  (:function shortname ((post post)) (or null string))
  "Returns the shortname of the post's author."
  (:function (setf shortname)
             ((new-value (or null string)) (post post)) (or null string))
  "Sets the shortname of the post's author."
  (:function date ((post post)) date)
  "Returns the date of the post."
  (:function (setf date) ((new-value date) (post post)) date)
  "Sets the date of the post."
  (:function contents ((post post)) string)
  "Returns the contents of the post."
  (:function (setf contents) ((new-value string) (post post)) string)
  "Sets the contents of the post.")

(execute-protocol post)
