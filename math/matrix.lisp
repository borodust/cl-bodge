(in-package :cl-bodge.math)


(defmethod multiply ((this mat4) (that mat4))
  (make-instance 'mat4 :value (sb-cga:matrix* (value-of this) (value-of that))))


(definline mat->array (mat)
  (value-of mat))


(definline mref (mat row column)
  (sb-cga:mref (value-of mat) row column))


(definline (setf mref) (value mat row column)
  (setf (sb-cga:mref (value-of mat) row column) value))


(definline identity-mat4 ()
  (make-instance 'mat4 :value (sb-cga:identity-matrix)))


(definline rotation-mat4* (x y z)
  (make-instance 'mat4 :value (sb-cga:rotate* x y z)))


(definline rotation-mat4 (vec)
  (make-instance 'mat4 :value (sb-cga:rotate (value-of vec))))


(definline translation-mat4* (x y z)
  (make-instance 'mat4 :value (sb-cga:translate* x y z)))


(definline translation-mat4 (vec)
  (make-instance 'mat4 :value (sb-cga:translate (value-of vec))))


(definline scaling-mat4* (x y z)
  (make-instance 'mat4 :value (sb-cga:scale* x y z)))


(defun make-mat3 (m11 m12 m13
                  m21 m22 m23
                  m31 m32 m33)
  (make-instance 'mat3 :value (make-array 9
                                          :element-type 'single-float
                                          :initial-contents (list m11 m21 m31
                                                                  m12 m22 m32
                                                                  m13 m23 m33))))


(defun mat4->mat3 (mat4)
  (macrolet ((%mat4->mat3 (m4 m3)
               (once-only (m4 m3)
                 `(progn
                    ,@(loop for i from 0 below 3 append
                           (loop for j from 0 below 3 collect
                                `(setf (aref ,m3 ,(+ (* i 3) j)) (aref ,m4 ,(+ (* i 4) j)))))))))
    (let ((m3 (make-array 9 :element-type 'single-float :initial-element 0f0)))
      (%mat4->mat3 (value-of mat4) m3)
      (make-instance 'mat3 :value m3))))


(declaim (ftype (function (single-float single-float single-float single-float) *)
                perspective-projection-mat)
         (inline perspective-projection-mat))
(defun perspective-projection-mat (width height near far)
  (let ((x0 (/ near width 1/2))
        (y1 (/ near height 1/2))
        (z2 (/ (+ near far) (- near far)))
        (z3 (/ (* 2 far near) (- near far))))
    (make-instance 'mat4 :value (sb-cga:matrix x0   0f0  0f0  0f0
                                               0f0  y1   0f0  0f0
                                               0f0  0f0  z2   z3
                                               0f0  0f0  -1f0 0f0))))
