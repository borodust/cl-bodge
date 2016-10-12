(in-package :cl-bodge.physics)


(defclass mass (disposable)
  ((value :initform (cffi:foreign-alloc '(:struct ode:mass)) :reader value-of)))


(define-destructor mass (value)
  (cffi:foreign-free value))


(defun make-box-mass (total x y z)
  (let ((mass (make-instance 'mass)))
    (ode:mass-set-box-total (value-of mass) total x y z)
    mass))
