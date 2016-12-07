(in-package :cl-bodge.physics)


(defclass mass (disposable)
  ((value :initform (calloc '%ode:mass) :reader value-of)))


(define-destructor mass (value)
  (free value))


(defun make-box-mass (total x y z)
  (let ((mass (make-instance 'mass)))
    (%ode:mass-set-box-total (value-of mass) total x y z)
    mass))


(defun make-sphere-mass (total radius)
  (let ((mass (make-instance 'mass)))
    (%ode:mass-set-sphere-total (value-of mass) total radius)
    mass))
