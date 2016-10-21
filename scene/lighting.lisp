(in-package :cl-bodge.scene)


(defclass directional-light-node (shading-parameters-node) ())


(defmethod initialize-instance ((this directional-light-node)
                                &key ambient diffuse direction)
  (call-next-method this :parameters `(("dLight.ambient" . ,ambient)
                                       ("dLight.diffuse" . ,diffuse)
                                       ("dLight.direction" . ,direction))))
