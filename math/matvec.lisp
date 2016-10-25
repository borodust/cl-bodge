(in-package :cl-bodge.math)


(defgeneric mv* (mat vec))


(defmethod mv* ((this-mat mat3) (this-vec vec3))
  (macrolet ((mul (m v r)
               (once-only (m v r)
                 `(progn
                    ,@(loop for i from 0 below 3 collect
                           `(setf (aref ,r ,i)
                                  (+ ,@(loop for j from 0 below 3 collect
                                            `(* (aref ,v ,j)
                                                (aref ,m ,(+ (* j 3) i)))))))))))
    (let ((r (make-vec3*)))
      (mul (value-of this-mat) (value-of this-vec) (value-of r))
      r)))
