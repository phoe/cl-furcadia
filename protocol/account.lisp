;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; account.lisp

(in-package :cl-furcadia/protocol)

;;; TODO does the protocol-redefining code in PROTEST even make sense?
;;; TODO define a way in PROTEST to make tests that are run for all subclasses
;;; of the protocol class
(define-protocol account
    (:description "The ACCOUNT protocol describes objects representing ~
accounts for the MMOSG Furcadia. These accounts can, among others, log into ~
the Furcadia web services, access, modify and launch game characters (called ~
furres), and make in-game purchases.
\
Each account is uniquely identified by its associated email address.
\
Indirect instances of this class should be returned every time client code ~
makes a successful login to Furcadia web services."
     :tags (:cl-furcadia :account)
     :export t)
  (:class account () ())
  "An account object. Each class participating in this protocol must subclass ~
this protocol class."
  (:function email ((account account)) string)
  "Returns the email of the account. This value should be provided when ~
instantiating the account and is otherwise immutable."
  (:function password ((account account)) string)
  "Returns the password of the account."
  (:function (setf password) ((new-value string) (account account)) string)
  "Sets the password of the account."
  (:function id ((account account)) unsigned-byte)
  "Returns the ID of the account."
  (:function (setf id)
             ((new-value unsigned-byte) (account account)) unsigned-byte)
  "Sets the ID of the account."
  (:function main ((account account)) (or furre null))
  "Returns the main character of the account."
  (:function (setf main)
             ((new-value (or furre null)) (account account)) (or furre null))
  "Sets the main character of the account."
  (:function gd ((account account)) unsigned-byte)
  "Returns the Dragonscales available on the account."
  (:function (setf gd)
             ((new-value rational) (account account)) rational)
  "Sets the Dragonscales available on the account."
  (:function furres ((account account)) list)
  "Returns a list of all furres on the account. The returned list must be a ~
proper list of objects of type FURRE."
  (:function (setf furres) ((new-value list) (account account)) list)
  "Sets a list of all furres on the account. The assigned list must be a ~
proper list of objects of type FURRE."
  (:function session ((account account)) (or string null))
  "Returns the Furcadia Web Services session identifier associated with the ~
account."
  (:function (setf session)
             ((new-value (or string null)) (account account)) (or string null))
  "Sets the Furcadia Web Services session identifier associated with the ~
account.")
