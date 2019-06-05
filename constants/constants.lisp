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
  "A list of all symbols denoting Furcadia color types."
  '#.`(member ,@*color-types*))

(deftype wings ()
  "A list of all symbols denoting Furcadia wing types."
  '#.`(member ,@*wings*))

(defun wings-name (wings)
  "Given a wing type, returns its proper name in form of a string."
  (check-type wings wings)
  (if (null wings)
      "No Wings"
      (cat (string-capitalize (string wings)) " Wings")))

;;; Data files

(defvar *digos*
  (read-data-file :cl-furcadia.constants "data/digos.lisp")
  "Hash-table containing digo data. The keys are unsigned-bytes and the values
are instances of CL-FURCADIA/CLOS:DIGO class.")

(defparameter *gradients*
  (read-data-file :cl-furcadia.constants "data/gradients.lisp")
  "Hash-table containing gradient data. The keys are in form (SYMBOL STRING),
where SYMBOL is taken from *COLOR-TYPES* and STRING is a valid color name.")

(maphash (lambda (key value) (declare (ignore value))
           (coercef (gethash key *gradients*) '(vector (unsigned-byte 8))))
         *gradients*)

(defvar *color-names*
  (read-data-file :cl-furcadia.constants "data/color-names.lisp")
  "Hash-table containing color names. Keys are symbols from *COLOR-TYPES*.")

(defvar *classic-palette*
  (read-data-file :cl-furcadia.constants "data/classic-palette.lisp")
  "Furcadia classic palette in RGBA format.")

(defvar *legacy-remaps*
  (read-data-file :cl-furcadia.constants "data/legacy-remaps.lisp")
  "Hashtable from legacy remap colors to legacy remap types. Keys are indices on
the classic palette. Values are two-element lists in form of (KEYWORD INDEX),
where KEYWORD is a legacy remap type and INDEX is an index on this color's
brightness (0 is brightest).")

;;; Other variables

(defvar *file-generators*
  '((0 . nil) (1 . :fured) (2 . :uploader) (3 . :haxe-client)
    (4 . :html-client) (5 . :php) (72 . :apple) (200 . :raptor-fox5))
  "A list of all known FOX5 file generators.")

(defvar *color-values*
  '((:badge . 1) (:cape . 2) (:eyes . 3) (:markings . 4) (:vest . 6)
    (:accent . 7) (:bracers . 9) (:wings . 10) (:trousers . 12) (:hair . 13)
    (:boots . 14) (:fur . 15) (:outline . 255))
  "Alist between remap types and their respective integers")

(defvar *legacy-remap-types*
  '((:badge . 1)  (:cape . 2) (:eyes . 3) (:markings . 4) (:vest . 6)
    (:bracers . 9) (:trousers . 12) (:hair . 13) (:boots . 14) (:fur . 15)
    (:outline . 255) (:shadow . :shadow))
  "An alist containing a mapping from legacy remap types to modern remap
codes. CARs are valid as partial input to *LEGACY-REMAPS*.")

(defvar *color-code-indices*
  '(:version :fur :markings :hair :eyes :badge :vest :bracers :cape
    :boots :trousers :wings :accent :gender :species :reserved)
  "Indices of a Furcadia color code.")

(defvar *genders*
  '(:female :male :unspecified)
  "Gender values possible in a Furcadia color code.")

(defvar *wingable-digos*
  '(1 2 3 4 5 6 7 8 9 10 96 120 121 127 131 132 139 149 159 188 228 234 257)
  "Identifiers for digos which are capable of displaying wings.")

(defvar *gradient-stops*
  '(24 48 72 96 120 144 168 192)
  "The gradient stops used for remapping.")

(defvar *gradient-stops-hair*
  '(24 48 72 96 192)
  "The gradient stops used for remapping hair.")

(defvar *gradient-blends*
  '((16 :fur :markings) (21 :fur :hair) (26 :fur :wings)
    (31 :markings :hair) (36 :markings :wings) (41 :hair :wings))
  "All blends defined in the FOX5 specification, in form of (STARTING-SLIDE
COLOR-1 COLOR-2).")

(defvar *kitterspeak*
  '((2 . :delay) (3 . :loop) (4 . :jump) (5 . :frame-x) (6 . :frame-y)
    (7 . :furre-x) (8 . :furre-y) (11 . :auto-delay) (12 . :stop)
    (13 . :camera-follow-furre-p) (14 . :random-auto-delay)
    (15 . :random-delay) (17 . :opacity) (18 . :slide-frame-x)
    (19 . :slide-frame-y) (20 . :slide-furre-x) (21 . :slide-furre-y)
    (22 . :slide-opacity) (23 . :show-bg-frame) (24 . :show-fg-frame)
    (29 . :show-behind-frame) (30 . :show-front-frame) (31 . :move-forward)
    (32 . :move-backward) (33 . :effect-layer-mode)
    ;; legacy
    (1 . :show-frame) (9 . :move-to-front) (10 . :move-behind)
    ;; deprecated
    (16 . :shape-frame) (25 . :show-bg-object) (26 . :show-fg-object)
    (27 . :hide-bg) (28 . :hide-fg))
  "The alist of Kitterspeak step/rule codes and their names.
\
Rules 14+ seem to be unused in FOX1.
Rules 1, 9, 10 are legacy and their usage in new Kitterspeak lines is
discouraged.
Rules 16, 25, 26, 27, 28 are deprecated and not meant to be used.")

(defvar *desc-standards*
  '((100 . :e) (200 . :t+) (300 . :m16+) (400 . :a18+) (500 . :ao) (600 . :aoc))
  "Al alist of age standards available for Furcadia descriptions.")
