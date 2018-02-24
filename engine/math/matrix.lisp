(cl:in-package :cl-bodge.math)


(definline mat->array (mat)
  (value-of mat))


(defgeneric mref (mat row column))


(defmethod mref ((this mat2) row column)
  (aref (value-of this) (+ row (* column 2))))


(defmethod mref ((this mat3) row column)
  (m3:melm (value-of this) row column))


(defmethod mref ((this mat4) row column)
  (m4:melm (value-of this) row column))


(defgeneric (setf mref) (value mat row column))


(defmethod (setf mref) (value (this mat2) row column)
  (setf (aref (value-of this) (+ row (* column 2))) (f value)))


(defmethod (setf mref) (value (this mat3) row column)
  (setf (m3:melm (value-of this) row column) (f value)))


(defmethod (setf mref) (value (this mat4) row column)
  (setf (m4:melm (value-of this) row column) (f value)))


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


(definline euler-angles->mat3 (vec3)
  (make-instance 'mat3 :value (m3:rotation-from-euler (value-of vec3))))


(defun angle->mat2 (angle)
  (let ((val (make-array 4 :element-type 'single-float))
        (sin (sin angle))
        (cos (cos angle)))
    (setf (aref val 0) cos
          (aref val 1) sin
          (aref val 2) (- sin)
          (aref val 3) cos)
    (make-instance 'mat2 :value val)))


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


(defun mat4 (m11 m12 m13 m14
             m21 m22 m23 m24
             m31 m32 m33 m34
             m41 m42 m43 m44)
  (make-instance 'mat4 :value (m4:make (f m11) (f m12) (f m13) (f m14)
                                       (f m21) (f m22) (f m23) (f m24)
                                       (f m31) (f m32) (f m33) (f m34)
                                       (f m41) (f m42) (f m43) (f m44))))


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


(defun mat3->mat4 (mat3)
  (macrolet ((%mat3->mat4 (m3 m4)
               (once-only (m3 m4)
                 `(progn
                    ,@(loop for i from 0 below 3 append
                           (loop for j from 0 below 3 collect
                                `(setf (aref ,m4 ,(+ (* i 4) j)) (aref ,m3 ,(+ (* i 3) j)))))))))
    (let ((m4 (m4:identity)))
      (%mat3->mat4 (value-of mat3) m4)
      (make-instance 'mat4 :value m4))))


(defun rotation-translation->mat4 (mat3 vec3)
  (let ((result (mat3->mat4 mat3)))
    (setf (mref result 0 3) (x vec3)
          (mref result 1 3) (y vec3)
          (mref result 2 3) (z vec3))
    result))


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

(defmethod multiply ((this mat2) (that vec2))
  (let ((x (x that))
        (y (y that)))
    (vec2 (+ (* x (mref this 0 0)) (* y (mref this 0 1)))
          (+ (* x (mref this 1 0)) (* y (mref this 1 1))))))


(defmethod multiply ((this mat4) (that mat4))
  (make-instance 'mat4 :value (m4:* (value-of this) (value-of that))))


(defmethod multiply ((this mat4) (that vec4))
  (make-instance 'vec4 :value (m4:*v (value-of this) (value-of that))))


(defmethod multiply ((this mat3) (that mat3))
  (make-instance 'mat3 :value (m3:* (value-of this) (value-of that))))


(defmethod multiply ((this mat3) (that vec3))
  (make-instance 'vec3 :value (m3:*v (value-of this) (value-of that))))


(defun basis->mat4 (x-axis y-axis z-axis &optional (position (vec3 0.0 0.0 0.0)))
  (let ((result (m4:from-columns-v3 (value-of x-axis)
                                    (value-of y-axis)
                                    (value-of z-axis))))
    (setf (m4:melm result 0 0) (x position)
          (m4:melm result 1 0) (y position)
          (m4:melm result 2 0) (z position))))
