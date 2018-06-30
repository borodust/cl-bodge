(cl:in-package :cl-bodge.physics.ode)


(defmethod simulation-engine-make-universe ((engine ode-engine)
                                            &key on-pre-solve on-post-solve &allow-other-keys)
  (make-universe))


(defmethod simulation-engine-destroy-universe ((engine ode-engine) universe)
  (destroy-universe universe))


(defmethod simulation-engine-observe-universe ((engine ode-engine) universe time-step)
  (%observe-universe universe time-step))


(defmethod simulation-engine-gravity ((engine ode-engine) universe)
  (gravity universe))


(defmethod (setf simulation-engine-gravity) ((value vec3) (engine ode-engine) universe)
  (setf (gravity universe) value))
