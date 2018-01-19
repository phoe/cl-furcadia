;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-FURCADIA
;;;; © Michał "phoe" Herda 2017
;;;; remap.lisp

(in-package #:cl-furcadia/remap)

(defun 8bit-32bit (vector &optional remapp)
  "Converts the provided image data from 8-bit to 32-bit. If REMAPP is true, the
resulting image will be remappable."
  (let* ((length (array-total-size vector))
         (result (make-array (* 4 length) :element-type '(unsigned-byte 8)))
         (palette *classic-palette*))
    (labels ((pref (c i) (aref palette (+ i (* 4 c)))))
      (loop for i from 0 below length
            for color = (aref vector i)
            for (keyword index) = (gethash color *legacy-remaps*)
            if (and remapp keyword index)
              do (let ((g (assoc-value *legacy-remap-types* keyword)))
                   (setf (subseq result (* 4 i) (+ 4 (* 4 i)))
                         (if (eq g :shadow)
                             #(0 0 0 0)
                             `#(0 ,g ,(nth (- 7 index) *gradient-stops*) 255))))
            else
              do (setf (subseq result (* 4 i) (+ 4 (* 4 i)))
                       (if (= color 0)
                           #(0 0 0 0)
                           `#(,(let ((b (pref color 2))) (if (= 0 b) 1 b))
                              ,(pref color 1) ,(pref color 0) ,(pref color 3))))
            finally (return result)))))

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
                     (remap-pixel (subseq result i (+ i 4)) gradients)))
    result))

(defun remap-pixel (pixel gradients)
  "Remaps the provided pixel using the provided gradients. GRADIENTS should be
a hashtable in the same format as valid output from ALL-GRADIENTS."
  (declare (optimize speed))
  (destructuring-bind (b g r a) (coerce pixel 'list)
    (declare (type (unsigned-byte 8) b g r a))
    (cond
      ((/= b 0) pixel)
      ((= g 0) pixel)
      ((= g 255) #(0 0 0 255))
      ((not (nth-value 1 (gethash g gradients)))
       (warn "Unknown remap type ~D found." g) ;; TODO warn only once
       pixel)
      (t (let ((gradient (gethash g gradients)))
           (declare (type (vector t) gradient))
           (let ((color (subseq gradient (* r 4) (+ (* r 4) 4))))
             `#(,(aref color 2) ,(aref color 1) ,(aref color 0) ,a)))))))

(defun color-code-gradient (type code)
  "Given a color type and a color code, returns the respective gradient."
  (check-type type color)
  (let* ((char-position (position type *color-code-indices*))
         (char (char code char-position))
         (color-position (from-220 char))
         (names (gethash type *color-names*))
         (name (nth color-position names))
         (gradient (gethash (list type name) *gradients*)))
    gradient))

(defun all-gradients (color-code)
  "Provided a color code, returns a fresh hash-table containing a map between
all valid color indices and their respective gradients."
  (let ((result (make-hash-table)))
    (flet ((generate (type)
             (setf (gethash (assoc-value *color-values* type) result)
                   (color-code-gradient type color-code))))
      (mapc #'generate *color-types*)
      (add-all-blends result)
      result)))

(defun add-all-blends (hash-table)
  "Provided a hash-table with already defined base gradients, adds blending
gradients to that hash-table."
  (mapc (curry #'apply #'add-blends hash-table) *gradient-blends*))

(defun add-blends (hash-table start count color-1 color-2)
  (let ((iota (iota count)))
    (mapcar (lambda (i) (add-blend hash-table start i color-1 color-2)) iota)))

(defun add-blend (hash-table start position color-1 color-2)
  (let* ((index-1 (assoc-value *color-values* color-1))
         (index-2 (assoc-value *color-values* color-2))
         (slide-1 (gethash index-1 hash-table))
         (slide-2 (gethash index-2 hash-table))
         (blend (make-array (length slide-1) :element-type t)) ;; TODO ubyte8
         (factor (/ (1+ position) 6)))
    (loop for i from 0
          for x across slide-1
          for y across slide-2
          for lerp = (round (lerp factor x y))
          do (setf (aref blend i) lerp)
          finally (setf (gethash (+ start position) hash-table) blend))))
