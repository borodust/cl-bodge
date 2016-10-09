(in-package :cl-bodge.math)

(declaim (inline vec->array))
(defun vec->array (vec)
  vec)


(declaim (inline make-vec3))
(defun make-vec3 (&optional (x #f0) (y #f0) (z #f0))
  (sb-cga:vec x y z))


(declaim (inline sequence->vec3))
(defun sequence->vec3 (seq)
  (make-vec3 (elt seq 0)
             (elt seq 1)
             (elt seq 2)))


(declaim (ftype (function (vec integer) single-float) vref)
         (inline vref))
(defun vref (vec idx)
  (aref vec idx))


(declaim (ftype (function (single-float vec integer) single-float) (setf vref))
         (inline (setf vref)))
(defun (setf vref) (value vec idx)
  (setf (aref vec idx) value))
