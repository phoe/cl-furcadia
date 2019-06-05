;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2018
;;;; image.lisp

(in-package :cl-furcadia/protocol)

(define-protocol image
    (:documentation "The IMAGE protocol describes objects representing images ~
assigned to characters on the MMOSG Furcadia. These images are not an integral ~
part of Furcadia, but may be used by Furcadia-related utilities, such as ~
Raptor Launcher.
\
The image objects do not contain the actual image data; instead, they are ~
metadata, containing URLs that lead to the actual displayable images.
\
Image IDs are unique only among all images for a given character. The tuple ~
uniquely identifying the image is a tuple of the character's shortname, the ~
image ID, and the timestamp."
     :tags (:cl-furcadia :image)
     :dependencies (furre)
     :export t)
  (:class image () ())
  "An image object. Each class participating in this protocol must subclass ~
this protocol class."
  (:function furre ((image image)) (or null furre))
  "Returns the furre associated with the image."
  (:function (setf furre)
             ((new-value (or null furre)) (image image)) (or null furre))
  "Sets the furre associated with the image."
  (:function iid ((image image)) unsigned-byte)
  "Returns the ID of the image."
  (:function (setf iid) ((new-value unsigned-byte) (image image)) unsigned-byte)
  "Sets the ID the image."
  (:function timestamp ((image image)) unsigned-byte)
  "Returns the timestamp of the image."
  (:function (setf timestamp)
             ((new-value unsigned-byte) (image image)) unsigned-byte)
  "Sets the timestamp of the image."
  (:function url ((image image)) string)
  "Returns the URL of the image."
  (:function (setf url) ((new-value string) (image image)) string)
  "Sets the URL of the image."
  (:function image-data ((image image) dl-path) t)
  "Returns the data of the image retrieved from the given download path."
  (:function (setf image-data) (new-value (image image) dl-path) t)
  "Sets the data of the image to the given download path."
  (:function eye-level ((image image)) float)
  "Returns the eye level of the image."
  (:function (setf eye-level) ((new-value float) (image image)) float)
  "Sets the eye level of the image."
  (:function sfwp ((image image)) boolean)
  "Returns whether time image is SFW."
  (:function (setf sfwp) ((new-value boolean) (image image)) boolean)
  "Returns whether time image is SFW.")

(execute-protocol image)
