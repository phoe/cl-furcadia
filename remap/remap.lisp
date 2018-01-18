;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; remap.lisp

(in-package #:cl-furcadia/remap)

(defun color-code-gradient (type code)
  "Given a color type and a color code, returns the respective gradient."
  (check-type type color)
  (let* ((char-position (position type *color-code-indices*))
         (char (aref code char-position))
         (color-position (from-220 char))
         (names (gethash type *color-names*))
         (name (nth color-position names))
         (gradient (gethash (list type name) *gradients*)))
    (values gradient name)))

(defun all-gradients (color-code)
  "Provided a color code, returns a fresh hashtable containing a map between
all valid color types and their respective gradients."
  (let ((result (make-hash-table)))
    (flet ((generate (type)
             (setf (gethash type result)
                   (multiple-value-list
                    (color-code-gradient type color-code)))))
      (mapc #'generate *color-types*)
      result)))

(defun remap (image-data color-code)
  "Provided an ARGB image data and a color code, returns a fresh copy of the
image data with all eligible pixels remapped."
  (check-type image-data vector)
  (check-type color-code (or null string))
  (let* ((gradients (all-gradients color-code))
         (length (length image-data))
         (result (make-array length :element-type '(unsigned-byte 8)
                                    :initial-contents image-data)))
    (loop for i from 0 below length by 4
          for b = (aref result (+ i 0))
          if (= b 0)
            do (setf (subseq result i (+ i 4))
                     (remap-argb (subseq result i (+ i 4)) gradients)))
    result))

(defun remap-argb (vector gradients)
  (destructuring-bind (b g r a) (coerce vector 'list)
    (cond
      ((/= b 0) (list b g r a))
      ((= g 255) (list 0 0 0 255))
      (t (let ((type (gethash g *color-values*)))
           (unless type (error "No remap type ~D found." g))
           (let* ((gradient (first (gethash type gradients)))
                  (color (subseq gradient (* r 4) (+ (* r 4) 4))))
             (list (aref color 2) (aref color 1) (aref color 0) a)))))))

(defun 8bit-32bit (vector &optional remapp)
  (let* ((length (array-total-size vector))
         (result (make-array (* 4 length) :element-type '(unsigned-byte 8)))
         (palette *classic-palette*))
    (loop for i from 0 below length
          for color = (aref vector i)
          for remap = (gethash color *legacy-remaps*)
          if (and remapp remap)
            do (let ((g (assoc-value *legacy-remap-types* (first remap))))
                 (if (eq g :shadow)
                     (setf (subseq result (* 4 i) (+ 4 (* 4 i))) #(0 0 0 0))
                     (setf (aref result (+ 0 (* 4 i))) ;; B
                           0
                           (aref result (+ 1 (* 4 i))) ;; G
                           g
                           (aref result (+ 2 (* 4 i))) ;; R
                           (nth (- 7 (second remap)) *gradient-stops*)
                           (aref result (+ 3 (* 4 i))) ;; A
                           (aref palette (+ 3 (* 4 color))))))
          else
            unless (= color 0)
              do (setf (aref result (+ 0 (* 4 i))) ;; B
                       (aref palette (+ 2 (* 4 color)))
                       (aref result (+ 1 (* 4 i))) ;; G
                       (aref palette (+ 1 (* 4 color)))
                       (aref result (+ 2 (* 4 i))) ;; R
                       (aref palette (+ 0 (* 4 color)))
                       (aref result (+ 3 (* 4 i))) ;; A
                       (aref palette (+ 3 (* 4 color))))
          finally (return result))))
