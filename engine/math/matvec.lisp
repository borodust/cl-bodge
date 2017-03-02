(in-package :cl-bodge.math)


(defmethod multiply ((this-mat mat3) (this-vec vec3))
  (make-instance 'vec3 :value (m3:*v (value-of this-mat) (value-of this-vec))))


(defun basis->mat4 (x-axis y-axis z-axis &optional (position (vec3 0.0 0.0 0.0)))
  (let ((result (m4:from-columns-v3 (value-of x-axis)
                                    (value-of y-axis)
                                    (value-of z-axis))))
    (setf (m4:melm result 0 0) (x position)
          (m4:melm result 1 0) (y position)
          (m4:melm result 2 0) (z position))))
