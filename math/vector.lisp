(in-package :cl-bodge.math)


(definline vec->array (vec)
  (value-of vec))


(definline make-vec3* (&optional (x #f0) (y #f0) (z #f0))
  (make-instance 'vec3 :value (sb-cga:vec x y z)))


(definline make-vec4* (&optional (x #f0) (y #f0) (z #f0) (w #f0))
  (make-instance 'vec4 :value (sb-cga:vec4 x y z w)))


(definline make-vec2* (&optional (x #f0) (y #f0))
  (make-instance 'vec2 :value (sb-cga:vec2 x y)))


(definline make-vec3 (vec)
  (make-instance 'vec3 :value (copy-array (value-of vec))))


(definline sequence->vec3 (seq)
  (make-vec3* (elt seq 0)
              (elt seq 1)
              (elt seq 2)))


(declaim (ftype (function (vec (integer 0 3)) single-float) vref)
         (inline vref))
(defun vref (vec idx)
  (aref (value-of vec) idx))


(declaim (ftype (function (single-float vec (integer 0 3)) single-float) (setf vref))
         (inline (setf vref)))
(defun (setf vref) (value vec idx)
  (setf (aref (value-of vec) idx) value))


(defgeneric v+ (vec &rest vectors))


(defmethod v+ ((this vec3) &rest others)
  (let ((sum (make-vec3 this)))
    (reduce (lambda (result val)
              (sb-cga:%vec+ result result (value-of val)))
            others
            :initial-value (value-of sum))
    sum))
