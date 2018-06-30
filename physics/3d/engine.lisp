(cl:in-package :cl-bodge.physics.ode)


(defclass ode-engine ()
  ())


(register-simulation-engine '(:3d :ode) (make-instance 'ode-engine))


(defmethod simulation-engine-initialize ((this ode-engine))
  (declare (ignore this)))


(defmethod simulation-engine-discard ((this ode-engine))
  (declare (ignore this)))
