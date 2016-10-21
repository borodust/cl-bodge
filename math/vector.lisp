(in-package :cl-bodge.math)


(definline vec->array (vec)
  (value-of vec))


(definline make-vec3 (&optional (x #f0) (y #f0) (z #f0))
  (make-instance 'vec3 :value (sb-cga:vec x y z)))


(definline sequence->vec3 (seq)
  (make-vec3 (elt seq 0)
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
