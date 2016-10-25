(in-package :cl-bodge.scene)


(declaim (special
          *animation*
          *bones*))


(defclass bone (node)
  (bone-id transform-mat))


(defmethod rendering-pass ((this bone)))
