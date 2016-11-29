(in-package :cl-bodge.math)


(defmethod lerp ((this number) (that number) f)
  (+ this (* (- that this) f)))
