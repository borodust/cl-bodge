(in-package :cl-bodge.physics)


(defstruct (physics-context
             (:conc-name ctx-))
  (universe (make-universe) :read-only t))


(declaim (inline universe))
(define-system-function universe physics-system ()
  (ctx-universe *system-context*))


(defclass physics-system (thread-bound-system) ())


(definline physics ()
  (engine-system 'physics-system))


(defmethod make-system-context ((this physics-system))
  (%ode:init-ode)
  (make-physics-context))


(defmethod destroy-system-context ((this physics-system) ctx)
  (destroy-universe (ctx-universe *system-context*))
  (%ode:close-ode))


(defun observe-universe (timestep)
  (%observe-universe (universe) timestep))


(define-system-function (setf gravity) physics-system (vec)
  (%ode:world-set-gravity (world-of (universe))
                          (ode-real (vref vec 0))
                          (ode-real (vref vec 1))
                          (ode-real (vref vec 2))))
