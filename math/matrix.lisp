(in-package :cl-bodge.math)

(declaim (ftype (function (&rest mat4) mat4) m*)
         (inline m*))
(defun m* (&rest matricies)
  (apply #'sb-cga:matrix* matricies))


(definline mat-dimensions (matrix)
  (array-dimensions matrix))


(definline mat->array (mat)
  mat)


(definline mref (mat row column)
  (sb-cga:mref mat row column))


(definline (setf mref) (value mat row column)
  (setf (sb-cga:mref mat row column) value))


(definline identity-mat4 ()
  (sb-cga:identity-matrix))


(definline rotation-mat4* (x y z)
  (sb-cga:rotate* x y z))


(definline rotation-mat4 (vec)
  (sb-cga:rotate vec))


(definline translation-mat4* (x y z)
  (sb-cga:translate* x y z))


(definline translation-mat4 (vec)
  (sb-cga:translate vec))


(definline scaling-mat4* (x y z)
  (sb-cga:scale* x y z))


(declaim (ftype (function (single-float single-float single-float single-float) mat4)
                perspective-projection-mat)
         (inline perspective-projection-mat))
(defun perspective-projection-mat (width height near far)
  (let ((x0 (/ near width 1/2))
        (y1 (/ near height 1/2))
        (z2 (/ (+ near far) (- near far)))
        (z3 (/ (* 2 far near) (- near far))))
    (sb-cga:matrix x0   0f0  0f0  0f0
                   0f0  y1   0f0  0f0
                   0f0  0f0  z2   z3
                   0f0  0f0  -1f0 0f0)))
