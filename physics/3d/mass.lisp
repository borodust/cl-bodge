(cl:in-package :cl-bodge.physics.ode)


(defmethod simulation-engine-make-mass-for-sphere ((engine ode-engine) mass (radius number))
  (make-sphere-mass mass radius))


(defmethod simulation-engine-make-mass-for-cuboid ((engine ode-engine) mass
                                                   (width number)
                                                   (height number)
                                                   (depth number))
  (make-box-mass mass width height depth))
