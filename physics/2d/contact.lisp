(cl:in-package :cl-bodge.physics.chipmunk)

(declaim (special *arbiter*))

(defmethod (setf simulation-engine-collision-friction) ((value number) (engine chipmunk-engine))
  (%cp:arbiter-set-friction *arbiter* (cp-float value))
  value)

(defmethod (setf simulation-engine-collision-elasticity) ((value number) (engine chipmunk-engine))
  (%cp:arbiter-set-restitution *arbiter* (cp-float value))
  value)

(defmethod (setf simulation-engine-collision-surface-velocity) ((value ge.ng:vec2)
                                                                (engine chipmunk-engine))
  (with-cp-vect (vec value)
    (%cp:arbiter-set-surface-velocity *arbiter* vec))
  value)

(defmethod simulation-engine-contact-normal ((engine chipmunk-engine))
  (declare (ignore engine)))
(defmethod simulation-engine-contact-point ((engine chipmunk-engine))
  (declare (ignore engine)))
(defmethod simulation-engine-contact-depth ((engine chipmunk-engine))
  (declare (ignore engine)))
