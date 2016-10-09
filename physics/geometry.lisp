(in-package :cl-bodge.physics)


(defclass geom ()
  ((id :initarg :id :reader id-of)))


(defun bind-geom (geom rigid-body)
  (ode:geom-set-body (id-of geom) (id-of rigid-body)))

;;;
;;;
;;;
(defclass sphere-geom (geom) ())


(defun make-sphere-geom (radius)
  (make-instance 'sphere-geom :id (ode:create-sphere (space-of (universe)) radius)))


;;;
;;;
;;;
(defclass box-geom (geom) ())


(defun make-box-geom (x y z)
  (make-instance 'box-geom :id (ode:create-box (space-of (universe)) x y z)))



;;;
;;;
;;;
(defclass plane-geom (geom) ())


(defun make-plane-geom (a b c &optional (z #f0))
  (make-instance 'plane-geom :id (ode:create-plane (space-of (universe)) a b c z)))


;;;
;;;
;;;
(defclass capped-cylinder-geom (geom) ())


(defun make-capped-cylinder-geom (radius length)
  (make-instance 'capped-cylinder-geom
                 :id (ode:create-cylinder (space-of (universe)) radius length)))


;;;
;;;
;;;
(defclass ray-geom (geom) ())


(defun make-ray-geom (length)
  (make-instance 'ray-geom :id (ode:create-ray (space-of (universe)) length)))
