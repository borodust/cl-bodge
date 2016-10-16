(in-package :cl-bodge.physics)

(defstruct (physics-context
             (:conc-name pc-))
  (universe (make-universe) :read-only t))


(declaim (inline universe))
(defun universe ()
  (pc-universe *system-context*))


(defclass physics-system (thread-bound-system) ())


(defmethod initialize-system :after ((this physics-system))
  (ode:init-ode))


(defmethod discard-system :after ((this physics-system))
  (ode:close-ode))


(defmethod make-system-context ((this physics-system))
  (make-physics-context))


(defmethod destroy-system-context (ctx (this physics-system))
  (destroy-universe (pc-universe *system-context*)))


(defun observe-universe (timestep)
  (%observe-universe (universe) timestep))
