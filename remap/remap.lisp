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

(defun remap-all (color-code &rest image-datas)
  "Provided an ARGB image data and a color code, returns a fresh copy of the
image data with all eligible pixels remapped. The second value returns all
unknown remaps codes that were encountered."
  (check-type color-code (or null string))
  (dolist (image-data image-datas) (check-type image-data vector))
  (let* ((gradients (all-gradients color-code)))
    (loop for image-data in image-datas
          for (value miss) = (multiple-value-list (%remap image-data gradients))
          collect value into values
          nconc miss into misses
          finally (return (values values misses)))))

(defun remap (color-code image-data)
  "Provided an ARGB image data and a color code, returns a fresh copy of the
image data with all eligible pixels remapped. The second value returns all
unknown remaps codes that were encountered."
  (check-type color-code (or null string))
  (check-type image-data vector)
  (let* ((gradients (all-gradients color-code)))
    (%remap image-data gradients)))

(defun %remap (image-data gradients)
  (let* ((length (length image-data))
         (result (make-array length :element-type '(unsigned-byte 8)
                                    :initial-contents image-data))
         (missing-remaps '()))
    (loop for i from 0 below length by 4
          for b = (aref result (+ i 0))
          for g = (aref result (+ i 1))
          if (and (= b 0) (/= g 0))
            do (when (and (/= g 255) (not (nth-value 1 (gethash g gradients))))
                 (pushnew g missing-remaps))
               (setf (subseq result i (+ i 4))
                     (remap-pixel (subseq result i (+ i 4)) gradients)))
    (values result missing-remaps)))

(defun remap-pixel (pixel gradients)
  "Remaps the provided pixel using the provided gradients. GRADIENTS should be
a hashtable in the same format as valid output from ALL-GRADIENTS."
  (declare (optimize speed))
  (destructuring-bind (b g r a) (coerce pixel 'list)
    (declare (type (unsigned-byte 8) b g r a))
    (cond
      ((/= b 0) pixel)
      ((= g 255) #(0 0 0 255))
      ((not (nth-value 1 (gethash g gradients))) pixel)
      (t (let ((gradient (gethash g gradients)))
           (declare (type (vector (unsigned-byte 8)) gradient))
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

;;; TODO this needs to be optimized like holy hell, make it use octet-vectors
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
  "Provided a hash-table with already defined base gradients, a start index, a
count of how many blends to add, and two color types to blend between, adds the
respective blends to the hash-table."
  (declare (optimize speed)
           (type symbol color-1 color-2)
           (type (unsigned-byte 8) start))
  (loop with iota = (loop for i of-type (unsigned-byte 3) below count collect i)
        for position of-type (unsigned-byte 3) in iota
        do (let ((index-1 (assoc-value *color-values* color-1))
                 (index-2 (assoc-value *color-values* color-2)))
             (declare (type (unsigned-byte 8) index-1 index-2))
             (let* ((slide-1 (gethash index-1 hash-table))
                    (slide-2 (gethash index-2 hash-table))
                    (length (length slide-1))
                    (blend (make-array length :element-type '(unsigned-byte 8)))
                    (factor (float (* (1+ position) (/ 6.0)))))
               (declare (type (simple-array (unsigned-byte 8) (*))
                              slide-1 slide-2))
               (loop for i from 0 below length
                     for x = (aref slide-1 i)
                     for y = (aref slide-2 i)
                     for lerp = (round (lerp factor x y))
                     do (setf (aref blend i) lerp)
                     finally (setf (gethash (+ start position) hash-table)
                                   blend))))))
