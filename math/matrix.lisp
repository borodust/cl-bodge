(in-package :cl-bodge.math)

(declaim (ftype (function (&rest mat4) mat4) m*)
         (inline m*))
(defun m* (&rest matricies)
  (apply #'sb-cga:matrix* matricies))


(declaim (inline matrix-dimensions))
(defun matrix-dimensions (matrix)
  (array-dimensions matrix))


(declaim (inline matrix->array))
(defun matrix->array (mat)
  mat)


(declaim (inline identity-matrix))
(defun identity-matrix ()
  (sb-cga:identity-matrix))


(declaim (inline rotation-matrix))
(defun rotation-matrix (x y z)
  (sb-cga:rotate* x y z))


(declaim (inline translation-matrix))
(defun translation-matrix (x y z)
  (sb-cga:translate* x y z))


(declaim (inline scaling-matrix))
(defun scaling-matrix (x y z)
  (sb-cga:scale* x y z))

(declaim (ftype (function (single-float single-float single-float single-float) mat4)
                perspective-projection-matrix)
         (inline perspective-projection-matrix))
(defun perspective-projection-matrix (width height near far)
  (let ((x0 (/ near width 1/2))
        (y1 (/ near height 1/2))
        (z2 (/ (+ near far) (- near far)))
        (z3 (/ (* 2 far near) (- near far))))
    (sb-cga:matrix x0   0f0  0f0  0f0
                   0f0  y1   0f0  0f0
                   0f0  0f0  z2   z3
                   0f0  0f0  -1f0 0f0)))
