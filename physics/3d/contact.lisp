(cl:in-package :cl-bodge.physics.ode)


(defmethod (setf simulation-engine-collision-friction) ((value number) (engine ode-engine))
  (setf (collision-friction *collision*) value))


(defmethod (setf simulation-engine-collision-elasticity) ((value number) (engine ode-engine))
  (setf (collision-elasticity *collision*) value))


(defmethod (setf simulation-engine-collision-surface-velocity) ((value number) (engine ode-engine))
  (setf (collision-surface-velocity *collision*) value))
