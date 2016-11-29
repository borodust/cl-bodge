(in-package :cl-bodge.math)


(defmethod multiply ((this-mat mat3) (this-vec vec3))
  (make-instance 'vec3 :value (m3:*v (value-of this-mat) (value-of this-vec))))
