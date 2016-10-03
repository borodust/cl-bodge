(in-package :cl-bodge.math)

(declaim (inline vector->array))
(defun vector->array (vec)
  vec)


(declaim (inline make-vec3))
(defun make-vec3 (&optional x y z)
  (sb-cga:vec x y z))


(declaim (inline sequence->vec3))
(defun sequence->vec3 (seq)
  (make-vec3 (elt seq 0)
             (elt seq 1)
             (elt seq 2)))
