;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; constants.lisp

(in-package #:cl-furcadia/constants)

;;; Types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *color-types*
    '(:fur :markings :hair :eyes :badge :vest :bracers
      :cape :boots :trousers :wings :accent)
    "All valid remappable color types in a Furcadia (sans outline)."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *wings*
    '(nil :classic :tri :butterfly :bat :prime :dragonfly)
    "Indices of Furcadia wing types."))

(deftype color ()
  "A symbol denoting a Furcadia color type."
  '#.`(member ,@*color-types*))

(deftype wings ()
  "A symbol denoting a Furcadia wing type."
  '#.`(member ,@*wings*))

;;; Data files

(defvar *digos*
  (read-data-file :cl-furcadia.constants "data/digos.lisp")
  "Hash-table containing digo data. The keys are unsigned-bytes and the values
are instances of CL-FURCADIA/CLOS:DIGO class.")

(defvar *gradients*
  (read-data-file :cl-furcadia.constants "data/gradients.lisp")
  "Hash-table containing gradient data. The keys are in form (SYMBOL STRING),
where SYMBOL is taken from *COLOR-TYPES* and STRING is a valid color name.")

(defvar *color-names*
  (read-data-file :cl-furcadia.constants "data/color-names.lisp")
  "Hash-table containing color names. Keys are symbols from *COLOR-TYPES*.")

(defvar *color-values*
  (read-data-file :cl-furcadia.constants "data/color-values.lisp")
  "Hash table between integers and their respective remap types.")

(defvar *classic-palette*
  (read-data-file :cl-furcadia.constants "data/classic-palette.lisp")
  "Furcadia classic palette in RGBA format.")

;;; Other variables

(defvar *color-code-indices*
  '(:version :fur :markings :hair :eyes :badge :vest :bracers :cape
    :boots :trousers :wings :accent :gender :species :reserved)
  "Indices of a Furcadia color code.")

(defvar *genders*
  '(:female :male :unspecified)
  "Gender values possible in a Furcadia color code.")

(defvar *wingable-digos*
  '(1 2 3 4 5 6 7 8 9 10 96 120 121 127 131 132 138 149 159 188 228 234 257)
  "Identifiers for digos which are capable of displaying wings.")

(defvar *gradient-stops*
  '(24 48 72 96 120 144 168 192)
  "The gradient stops used for remapping.")

(defvar *gradient-stops-hair*
  '(24 48 72 96 192)
  "The gradient stops used for remapping hair.")

(defvar *kitterspeak*
  '((1 . :frame) (2 . :delay) (3 . :loop) (4 . :jump) (5 . :posx) (6 . :posy)
    (7 . :furre-x) (8 . :furre-y)
    (9 . :draw-front) ;; legacy
    (10 . :draw-behind) ;; legacy
    (11 . :auto-frame-delay) (12 . :stop) (13 . :camera-state)
    (14 . :rand-frame-delay) (15 . :shape-frame)
    (16 . :shape-frame) ;; obsolete
    (17 . :opacity) (18 . :slide-posx) (19 . :slide-posy) (20 . :slide-furrex)
    (21 . :slide-furrey) (22 . :slide-opacity) (23 . :show-bgframe)
    (24 . :show-fgframe) (25 . :show-bgshape) (26 . :show-fgshape)
    (27 . :hide-bg) (28 . :hide-fg) (29 . :frame-behind) (30 . :frame-front)
    (31 . :move-forward) (32 . :move-backward))
  "The alist of Kitterspeak step/rule codes and their names.
\
Rules 14+ seem to be unused in FOX1.")

;;; Utility functions

(defun wings-name (wings)
  "Given a wing type, returns its proper name in form of a string."
  (check-type wings wings)
  (if (null wings)
      "No Wings"
      (cat (string-capitalize (string wings)) " Wings")))
