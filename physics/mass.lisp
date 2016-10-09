(in-package :cl-bodge.physics)


(defstruct (mass
             (:constructor make-mass ()))
  (value (cffi:foreign-alloc '(:struct ode:mass)) :read-only t))


(defun make-box-mass (total x y z)
  (let ((mass (make-mass)))
    (ode:mass-set-box-total (mass-value mass) total x y z)
    mass))
