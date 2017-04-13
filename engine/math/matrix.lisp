(in-package :cl-bodge.math)


(defmethod multiply ((this mat4) (that mat4))
  (make-instance 'mat4 :value (m4:* (value-of this) (value-of that))))


(defmethod multiply ((this mat4) (that vec4))
  (make-instance 'vec4 :value (m4:*v (value-of this) (value-of that))))


(definline mat->array (mat)
  (value-of mat))


(definline mref (mat row column)
  (m:elt (value-of mat) row column))


(definline (setf mref) (value mat row column)
  ;; fixme: hax, use generic and m2/3/4:melm
  (let ((len (if (= (length mat) 16) 4 3)))
    (setf (aref mat (+ row (* column len))) (f value))))


(definline identity-mat4 ()
  (make-instance 'mat4 :value (m4:identity)))


(definline identity-mat3 ()
  (make-instance 'mat3 :value (m3:identity)))


(definline euler-axis->mat4 (a vec3)
  (make-instance 'mat4 :value (m4:rotation-from-axis-angle (value-of vec3) (f a))))


(defmethod inverse ((this mat4))
  (make-instance 'mat4 :value (m4:inverse (value-of this))))


(definline euler-angles->mat4 (vec3)
  (make-instance 'mat4 :value (m4:rotation-from-euler (value-of vec3))))


(definline sequence->rotation-mat4 (sequence)
  (make-instance 'mat4 :value (m4:rotation-from-euler (v3:make (f (elt sequence 0))
                                                               (f (elt sequence 1))
                                                               (f (elt sequence 2))))))

(defgeneric mat->rotation-mat4 (mat)
  (:method ((mat mat3))
    (make-instance 'mat4 :value (m4:from-mat3 (value-of mat)))))


(defgeneric make-mat3 (obj &key))


(defmethod make-mat3 ((that mat4) &key)
  (mat4->mat3 that))


(defun sequence->mat4 (sequence)
  "row major"
  (let ((val (m4:0!)))
    (macrolet ((%array->m4 (arr)
                 (once-only (arr)
                   `(setf
                     ,@(loop for i from 0 below 4 append
                            (loop for j from 0 below 4 append
                                 `((m4:melm val ,i ,j) (f (aref ,arr ,(+ (* i 4) j))))))))))
      (if (listp sequence)
          (%array->m4 (make-array 16 :initial-contents sequence))
          (%array->m4 sequence)))
    (make-instance 'mat4 :value val)))


(definline translation-mat4 (x y z)
  (make-instance 'mat4 :value (m4:translation (v3:make (f x) (f y) (f z)))))


(definline sequence->translation-mat4 (sequence)
  (translation-mat4 (elt sequence 0)
                    (elt sequence 1)
                    (elt sequence 2)))


(definline vec->translation-mat4 (vec)
  (translation-mat4 (x vec) (y vec) (z vec)))


(definline scaling-mat4 (x y z)
  (make-instance 'mat4 :value (m4:scale (v3:make (f x) (f y) (f z)))))


(definline vec->scaling-mat4 (vec)
  (scaling-mat4 (vref vec 0)
                (vref vec 1)
                (vref vec 2)))


(defun mat3 (m11 m12 m13
             m21 m22 m23
             m31 m32 m33)
  (make-instance 'mat3 :value (m3:make (f m11) (f m12) (f m13)
                                       (f m21) (f m22) (f m23)
                                       (f m31) (f m32) (f m33))))


(defun mat4->mat3 (mat4)
  (macrolet ((%mat4->mat3 (m4 m3)
               (once-only (m4 m3)
                 `(progn
                    ,@(loop for i from 0 below 3 append
                           (loop for j from 0 below 3 collect
                                `(setf (aref ,m3 ,(+ (* i 3) j)) (aref ,m4 ,(+ (* i 4) j)))))))))
    (let ((m3 (m3:0!)))
      (%mat4->mat3 (value-of mat4) m3)
      (make-instance 'mat3 :value m3))))


(declaim (ftype (function (single-float single-float single-float single-float) *)
                perspective-projection-mat)
         (inline perspective-projection-mat))
(defun perspective-projection-mat (width height near far)
  (let ((x0 (f (/ near width 1/2)))
        (y1 (f (/ near height 1/2)))
        (z2 (f (/ (+ near far) (- near far))))
        (z3 (f (/ (* 2 far near) (- near far)))))
    (make-instance 'mat4 :value (m4:make x0   0f0  0f0  0f0
                                         0f0  y1   0f0  0f0
                                         0f0  0f0  z2   z3
                                         0f0  0f0  -1f0 0f0))))


(definline orthographic-projection-mat (width height near far)
  (make-instance 'mat4 :value (rtg-math.projection:orthographic (f width) (f height)
                                                                (f near) (f far))))
