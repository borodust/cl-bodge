(in-package :cl-bodge.math)


(defmethod lerp ((this number) (that number) (f number))
  (+ this (* (- that this) (f f))))
