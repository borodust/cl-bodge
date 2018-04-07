(cl:in-package :cl-bodge.physics.chipmunk)


(defclass chipmunk-engine ()
  ())


(register-simulation-engine '(:2d :chipmunk) (make-instance 'chipmunk-engine))


(defmethod simulation-engine-initialize ((this chipmunk-engine))
  (declare (ignore this)))


(defmethod simulation-engine-discard ((this chipmunk-engine))
  (declare (ignore this)))
