(cl:in-package :cl-bodge.physics.ode)


(defclass mass (disposable)
  ((value :initform (claw:calloc '%ode:mass) :reader value-of)))


(define-destructor mass (value)
  (claw:free value))


(defun make-box-mass (total x y z &optional offset)
  (let ((mass (make-instance 'mass)))
    (%ode:mass-set-box-total (value-of mass) (ode-real total)
                             (ode-real x) (ode-real y) (ode-real z))
    (when offset
      (%ode:mass-translate (value-of mass)
                           (ode-real (x offset))
                           (ode-real(y offset))
                           (ode-real (z offset))))
    mass))


(defun make-sphere-mass (total radius &optional offset)
  (let ((mass (make-instance 'mass)))
    (%ode:mass-set-sphere-total (value-of mass) (ode-real total) (ode-real radius))
    (when offset
      (%ode:mass-translate (value-of mass)
                           (ode-real (x offset))
                           (ode-real(y offset))
                           (ode-real (z offset))))
    mass))
