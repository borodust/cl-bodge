(cl:in-package :cl-bodge.physics.ode)


(defmethod simulation-engine-make-universe ((engine ode-engine)
                                            &key on-pre-solve on-post-solve &allow-other-keys))


(defmethod simulation-engine-destroy-universe ((engine ode-engine) universe))


(defmethod simulation-engine-observe-universe ((engine ode-engine) universe time-step))


(defmethod simulation-engine-gravity ((engine ode-engine) universe))


(defmethod (setf simulation-engine-gravity) ((value vec3) (engine ode-engine) universe))
