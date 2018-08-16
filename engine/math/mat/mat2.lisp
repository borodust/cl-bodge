(cl:in-package :cl-bodge.math)


(definline mat2 (m11 m12 m21 m22)
  (make-wrapped 'mat2 (m2:make (f m11) (f m12)
                               (f m21) (f m22))))


(defmethod mref ((this mat2) row column)
  (m2:melm (value-of this) row column))


(defmethod (setf mref) (value (this mat2) row column)
  (setf (m2:melm (value-of this) row column) (f value)))


(definline identity-mat2 ()
  (make-wrapped 'mat2 (m2:identity)))


(definline copy-mat2 (value)
  (make-wrapped 'mat2 (m2:copy-mat2 (value-of value))))


(defgeneric value->mat2 (value &key &allow-other-keys))


(defmethod value->mat2 ((this vec4) &key)
  (mat2 (x this) (y this)
        (z this) (w this)))


(defmethod value->mat2 ((this mat2) &key)
  (copy-mat2 this))


(defmethod value->mat2 ((this mat3) &key)
  (mat2 (mref this 0 0) (mref this 0 1)
        (mref this 1 0) (mref this 1 1)))


(defmethod value->mat2 ((this mat4) &key)
  (mat2 (mref this 0 0) (mref this 0 1)
        (mref this 1 0) (mref this 1 1)))


(definline euler-angle->mat2 (angle)
  (make-wrapped 'mat2 (m2:rotation-from-euler (f angle))))


(defun sequence->mat2 (sequence)
  "Matrix must be serialized in sequence in row major order"
  (let ((val (m2:0!)))
    (macrolet ((%array->m2 (arr)
                 (once-only (arr)
                   `(setf
                     ,@(loop for i from 0 below 2 append
                            (loop for j from 0 below 2 append
                                 `((m2:melm val ,i ,j) (f (aref ,arr ,(+ (* i 2) j))))))))))
      (if (listp sequence)
          (%array->m2 (make-array 4 :initial-contents sequence))
          (%array->m2 sequence)))
    (make-wrapped 'mat2 val)))


(definline scaling-mat2 (x y)
  (mat2 x 0
        0 y))


(defmethod transpose ((this mat2))
  (make-wrapped 'mat2 (m2:transpose (value-of this))))


(defmethod addere ((this mat2) (that mat2))
  (make-wrapped 'mat2 (m2:+ (value-of this) (value-of that))))


(defmethod addere ((this mat2) (that number))
  (mat2 (+ (mref this 0 0) that) (+ (mref this 0 1) that)
        (+ (mref this 1 0) that) (+ (mref this 1 1) that)))


(defmethod addere ((that number) (this mat2))
  (addere this that))


(defmethod subtract ((this mat2) (that mat2))
  (make-wrapped 'mat2 (m2:- this that)))


(defmethod subtract ((this mat2) (that number))
  (addere this (- that)))


(defmethod subtract ((that number) (this mat2))
  (multiply (subtract this that) -1))


(defmethod multiply ((this mat2) (that vec2))
  (make-wrapped 'mat2 (m2:*v (value-of this) (value-of that))))


(defmethod multiply ((this mat2) (that number))
  (make-wrapped 'mat2 (m2:*s (value-of this) (f that))))


(defmethod multiply ((that number) (this mat2))
  (multiply this that))


(defmethod divide ((this mat2) (that number))
  (multiply this (/ 1 that)))


(defmethod divide ((that number) (this mat2))
  (multiply (/ 1 that) this))


(defun basis->mat2 (x-axis y-axis)
  (make-wrapped 'mat2 (m2:from-columns x-axis y-axis)))
