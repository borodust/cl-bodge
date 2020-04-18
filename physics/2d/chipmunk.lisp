(cl:in-package :cl-bodge.physics.chipmunk)


(defvar *observing-p* nil)
(defvar *post-observation-hooks* nil)

(defclass chipmunk-engine ()
  ())


(register-simulation-engine '(:2d :chipmunk) (make-instance 'chipmunk-engine))


(defmethod simulation-engine-initialize ((this chipmunk-engine))
  (declare (ignore this)))


(defmethod simulation-engine-discard ((this chipmunk-engine))
  (declare (ignore this)))


(defun invoke-between-observations (hook)
  (if *observing-p*
      (push hook *post-observation-hooks*)
      (funcall hook)))
