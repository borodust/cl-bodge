(cl:in-package :cl-bodge.physics.ode)


(defmethod simulation-engine-make-rigid-body ((engine ode-engine) universe &key mass kinematic)
  (make-rigid-body universe kinematic))


(defmethod simulation-engine-destroy-rigid-body ((engine ode-engine) rigid-body)
  (dispose rigid-body))


(defmethod simulation-engine-apply-force ((engine ode-engine) body (force vec3))
  (apply-force body force))


(defmethod simulation-engine-body-force ((engine ode-engine) body)
  (body-force body))


(defmethod simulation-engine-apply-torque ((engine ode-engine) body (torque vec3))
  (apply-torque body torque))


(defmethod simulation-engine-body-torque ((engine ode-engine) body)
  (body-torque body))


(defmethod simulation-engine-body-mass ((engine ode-engine) body)
  (mass-of body))


(defmethod (setf simulation-engine-body-mass) ((value mass) (engine ode-engine) body)
  (setf (mass-of body) value))


(defmethod simulation-engine-body-position ((engine ode-engine) body)
  (position-of body))


(defmethod (setf simulation-engine-body-position) ((value vec3) (engine ode-engine) body)
  (setf (position-of body) value))


(defmethod simulation-engine-body-linear-velocity ((engine ode-engine) body)
  (linear-velocity-of body))


(defmethod (setf simulation-engine-body-linear-velocity) ((value vec3) (engine ode-engine) body)
  (setf (linear-velocity-of body) value))


(defmethod simulation-engine-body-rotation ((engine ode-engine) body)
  (rotation-of body))


(defmethod (setf simulation-engine-body-rotation) ((value mat3) (engine ode-engine) body)
  (setf (rotation-of body) value))


(defmethod simulation-engine-body-angular-velocity ((engine ode-engine) body)
  (angular-velocity-of body))


(defmethod (setf simulation-engine-body-angular-velocity) ((value vec3) (engine ode-engine) body)
  (setf (angular-velocity-of body) value))
