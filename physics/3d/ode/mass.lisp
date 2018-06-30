(cl:in-package :cl-bodge.physics.ode)


(defclass mass (disposable)
  ((value :initform (claw:calloc '%ode:mass) :reader value-of)))


(define-destructor mass (value)
  (claw:free value))


(defun make-box-mass (total x y z)
  (let ((mass (make-instance 'mass)))
    (%ode:mass-set-box-total (value-of mass) (ode-real total)
                             (ode-real x) (ode-real y) (ode-real z))
    mass))


(defun make-sphere-mass (total radius)
  (let ((mass (make-instance 'mass)))
    (%ode:mass-set-sphere-total (value-of mass) (ode-real total) (ode-real radius))
    mass))
