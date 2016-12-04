(in-package :cl-bodge.scene)


(defclass simulation-pass (system-scene-pass) ())


(defmethod initialize-instance ((this simulation-pass) &key)
  (call-next-method this :system (physics)))


(defun make-simulation-pass ()
  (make-instance 'simulation-pass))
