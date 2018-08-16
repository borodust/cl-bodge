(cl:in-package :cl-bodge.math)


(definline mat4 (m11 m12 m13 m14
                     m21 m22 m23 m24
                     m31 m32 m33 m34
                     m41 m42 m43 m44)
  (make-wrapped 'mat4 (m4:make (f m11) (f m12) (f m13) (f m14)
                               (f m21) (f m22) (f m23) (f m24)
                               (f m31) (f m32) (f m33) (f m34)
                               (f m41) (f m42) (f m43) (f m44))))


(definline identity-mat4 ()
  (make-instance 'mat4 :value (m4:identity)))


(definline copy-mat4 (value)
  (make-wrapped 'mat4 (m4:copy-mat4 (value-of value))))


(defmethod mref ((this mat4) row column)
  (m4:melm (value-of this) row column))


(defmethod (setf mref) (value (this mat4) row column)
  (setf (m4:melm (value-of this) row column) (f value)))


(definline euler-axis->mat4-homo (a vec3)
  (make-wrapped 'mat4 (m4:rotation-from-axis-angle (value-of vec3) (f a))))


(defmethod inverse ((this mat4))
  (make-wrapped 'mat4 (m4:inverse (value-of this))))


(defmethod transpose ((this mat4))
  (make-wrapped 'mat4 (m4:transpose (value-of this))))


(definline euler-angles->mat4-homo (vec3)
  (make-wrapped 'mat4 (m4:rotation-from-euler (value-of vec3))))


(defgeneric value->mat4 (value &key &allow-other-keys))


(defmethod value->mat4 ((this mat2) &key
                                      ((:02 m02) 0f0) ((:03 m03) 0f0)
                                      ((:12 m12) 0f0) ((:13 m13) 0f0)
                                      ((:20 m20) 0f0) ((:21 m21) 0f0) ((:22 m22) 0f0) ((:23 m23) 0f0)
                                      ((:30 m30) 0f0) ((:31 m31) 0f0) ((:32 m32) 0f0) ((:33 m33) 0f0))
  (mat4 (mref this 0 0) (mref this 0 1) m02 m03
        (mref this 1 0) (mref this 1 1) m12 m13
        m20             m21             m22 m23
        m30             m31             m32 m33))


(defmethod value->mat4 ((this mat3) &key
                                      ((:03 m03) 0f0)
                                      ((:13 m13) 0f0)
                                      ((:23 m23) 0f0)
                                      ((:30 m30) 0f0) ((:31 m31) 0f0) ((:32 m32) 0f0) ((:33 m33) 0f0))
  (mat4 (mref this 0 0) (mref this 0 1) (mref this 0 2) m03
        (mref this 1 0) (mref this 1 1) (mref this 1 2) m13
        (mref this 2 0) (mref this 2 1) (mref this 2 2) m23
        m30             m31             m32             m33))


(defmethod value->mat4 ((this mat4) &key)
  (copy-mat4 this))


(defun sequence->mat4 (sequence)
  "Matrix must be serialized in sequence in row major order"
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
    (make-wrapped 'mat4 val)))


(definline translation-mat4-homo (x y z)
  (make-wrapped 'mat4 (m4:translation (v3:make (f x) (f y) (f z)))))


(definline vec->translation-mat4-homo (vec)
  (translation-mat4-homo (x vec) (y vec) (z vec)))


(definline scaling-mat4-homo (x y z)
  (make-wrapped 'mat4 (m4:scale (v3:make (f x) (f y) (f z)))))


(definline vec->scaling-mat4-homo (vec)
  (scaling-mat4-homo (vref vec 0) (vref vec 1) (vref vec 2)))


(defmethod inverse ((this mat4))
  (make-wrapped 'mat4 (m4:inverse (value-of this))))


(defmethod transpose ((this mat4))
  (make-wrapped 'mat4 (m4:transpose (value-of this))))


(defun rotation-translation->mat4-homo (mat3 vec3)
  (let ((result (value->mat4 mat3)))
    (setf (mref result 0 3) (x vec3)
          (mref result 1 3) (y vec3)
          (mref result 2 3) (z vec3))
    result))


(defun perspective-projection-mat (width height near far)
  (let ((x0 (f (/ near width 1/2)))
        (y1 (f (/ near height 1/2)))
        (z2 (f (/ (+ near far) (- near far))))
        (z3 (f (/ (* 2 far near) (- near far)))))
    (make-wrapped 'mat4 (m4:make x0   0f0  0f0  0f0
                                 0f0  y1   0f0  0f0
                                 0f0  0f0  z2   z3
                                 0f0  0f0  -1f0 0f0))))


(defun orthographic-projection-mat (width height near far)
  (make-wrapped 'mat4 (rtg-math.projection:orthographic (f width) (f height)
                                                        (f near) (f far))))


(defmethod multiply ((this mat4) (that mat4))
  (make-wrapped 'mat4 (m4:* (value-of this) (value-of that))))


(defmethod multiply ((this mat4) (that vec4))
  (make-wrapped 'vec4 (m4:*v (value-of this) (value-of that))))


(defmethod multiply ((this mat4) (that number))
  (make-wrapped 'mat4 (m4:*s (value-of this) (f that))))


(defmethod multiply ((that number) (this mat4))
  (multiply this that))


(defmethod divide ((this mat4) (that number))
  (multiply this (/ 1 that)))


(defmethod divide ((that number) (this mat4))
  (multiply (/ 1 that) this))


(defun basis->mat4-homo (x-axis y-axis z-axis)
  (make-wrapped 'mat4 (m4:from-columns-v3 (value-of x-axis)
                                          (value-of y-axis)
                                          (value-of z-axis))))



(defun basis->mat4 (x-axis y-axis z-axis &optional (w-axis (vec4 0.0 0.0 0.0)))
  (make-wrapped 'mat4 (m4:from-columns (value-of x-axis)
                                       (value-of y-axis)
                                       (value-of z-axis)
                                       (value-of w-axis))))
