(cl:in-package :cl-bodge.math)


(definline mat3 (m11 m12 m13
                     m21 m22 m23
                     m31 m32 m33)
  (make-wrapped 'mat3 (m3:make (f m11) (f m12) (f m13)
                               (f m21) (f m22) (f m23)
                               (f m31) (f m32) (f m33))))


(definline identity-mat3 ()
  (make-wrapped 'mat3 (m3:identity)))


(definline copy-mat3 (value)
  (make-wrapped 'mat3 (m3:copy-mat3 (value-of value))))


(defmethod mref ((this mat3) row column)
  (m3:melm (value-of this) row column))


(defmethod (setf mref) (value (this mat3) row column)
  (setf (m3:melm (value-of this) row column) (f value)))


(definline euler-axis->mat3 (a vec3)
  (make-wrapped 'mat3 (m3:rotation-from-axis-angle (value-of vec3) (f a))))


(definline euler-angles->mat3 (vec3)
  (make-wrapped 'mat3 (m3:rotation-from-euler (value-of vec3))))


(definline euler-angle->mat3-homo (angle)
  (mat3 (f (cos angle)) (f (- (sin angle))) 0f0
        (f (sin angle)) (f (cos angle))     0f0
        0f0             0f0                 1f0))


(defgeneric value->mat3 (value &key &allow-other-keys))


(defmethod value->mat3 ((this mat2) &key
                                      ((:02 m02) 0f0)
                                      ((:12 m12) 0f0)
                                      ((:20 m20) 0f0) ((:21 m21) 0f0) ((:22 m22) 0f0))
  (mat3 (mref this 0 0) (mref this 0 1) m02
        (mref this 1 0) (mref this 1 1) m12
        m20             m21             m22))


(defmethod value->mat3 ((this mat3) &key)
  (copy-mat3 this))


(defmethod value->mat3 ((this mat4) &key)
  (mat3 (mref this 0 0) (mref this 0 1) (mref this 0 2)
        (mref this 1 0) (mref this 1 1) (mref this 1 2)
        (mref this 2 0) (mref this 2 1) (mref this 2 2)))


(defun sequence->mat3 (sequence)
  "Matrix must be serialized in sequence in row major order"
  (let ((val (m3:0!)))
    (macrolet ((%array->m3 (arr)
                 (once-only (arr)
                   `(setf
                     ,@(loop for i from 0 below 3 append
                            (loop for j from 0 below 3 append
                                 `((m3:melm val ,i ,j) (f (aref ,arr ,(+ (* i 3) j))))))))))
      (if (listp sequence)
          (%array->m3 (make-array 9 :initial-contents sequence))
          (%array->m3 sequence)))
    (make-wrapped 'mat3 val)))


(definline translation-mat3-homo (x y)
  (make-wrapped 'mat3 (m3:make 1f0 0f0 (f x)
                               0f0 1f0 (f y)
                               0f0 0f0 1f0)))


(definline vec->translation-mat3-homo (vec)
  (translation-mat3-homo (x vec) (y vec)))


(definline scaling-mat3 (x y z)
  (mat3 (f x) 0f0 0f0
        0f0 (f y) 0f0
        0f0 0f0 (f z)))


(definline scaling-mat3-homo (x y)
  (scaling-mat3 x y 1f0))


(definline vec->scaling-mat3 (vec)
  (scaling-mat3 (vref vec 0) (vref vec 1) (vref vec 2)))


(definline vec->scaling-mat3-homo (vec)
  (scaling-mat3-homo (vref vec 0) (vref vec 1)))


(defmethod inverse ((this mat3))
  (make-wrapped 'mat3 (m3:affine-inverse (value-of this))))


(defmethod transpose ((this mat3))
  (make-wrapped 'mat3 (m3:transpose (value-of this))))


(defun rotation-translation->mat3-homo (mat2 vec2)
  (let ((result (value->mat3 mat2 :m22 1f0)))
    (setf (mref result 0 2) (x vec2)
          (mref result 1 2) (y vec2))
    result))


(defmethod addere ((this mat3) (that mat3))
  (make-wrapped 'mat3 (m3:+ (value-of this) (value-of that))))


(defmethod addere ((this mat3) (that number))
  (mat3 (+ (mref this 0 0) that) (+ (mref this 0 1) that) (+ (mref this 0 2) that)
        (+ (mref this 1 0) that) (+ (mref this 1 1) that) (+ (mref this 1 2) that)
        (+ (mref this 2 0) that) (+ (mref this 2 1) that) (+ (mref this 2 2) that)))


(defmethod addere ((that number) (this mat3))
  (addere this that))


(defmethod subtract ((this mat3) (that mat3))
  (make-wrapped 'mat3 (m3:- this that)))


(defmethod subtract ((this mat3) (that number))
  (addere this (- that)))


(defmethod subtract ((that number) (this mat3))
  (multiply (subtract this that) -1))


(defmethod multiply ((this mat3) (that vec3))
  (make-wrapped 'mat3 (m3:*v (value-of this) (value-of that))))


(defmethod multiply ((this mat3) (that number))
  (make-wrapped 'mat3 (m3:*s (value-of this) (f that))))


(defmethod multiply ((that number) (this mat3))
  (multiply this that))


(defmethod divide ((this mat3) (that number))
  (multiply this (/ 1 that)))


(defmethod divide ((that number) (this mat3))
  (multiply (/ 1 that) this))


(defun basis->mat3 (x-axis y-axis &optional (z-axis (vec3 0 0 0)))
  (make-wrapped 'mat3 (m3:from-columns (value-of x-axis) (value-of y-axis) (value-of z-axis))))


(defun basis->mat3-homo (x-axis y-axis)
  (basis->mat3 (value->vec3 x-axis) (value->vec3 y-axis) (vec3 0 0 1)))
