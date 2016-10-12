(in-package :cl-bodge.physics)


(defclass ode-object (disposable thread-bound-object)
  ((id :initarg :id :initform (error "id must be provided") :reader id-of)))
