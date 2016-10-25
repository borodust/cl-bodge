(in-package :cl-bodge.physics)

(defstruct (physics-context
             (:conc-name ctx-))
  (universe (make-universe) :read-only t))


(declaim (inline universe))
(defun universe ()
  (ctx-universe *system-context*))


(defclass physics-system (thread-bound-system) ())


(defmethod make-system-context ((this physics-system))
  (ode:init-ode)
  (make-physics-context))


(defmethod destroy-system-context (ctx (this physics-system))
  (destroy-universe (ctx-universe *system-context*))
  (ode:close-ode))


(defun observe-universe (timestep)
  (%observe-universe (universe) timestep))


(defun (setf gravity) (vec)
  (ode:world-set-gravity (world-of (universe))
                         (vref vec 0) (vref vec 1) (vref vec 2)))
