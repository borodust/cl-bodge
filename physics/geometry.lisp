(in-package :cl-bodge.physics)


(defclass geom (ode-object) ())



(define-destructor geom ((id id-of) (sys system-of))
  (-> sys
    (ode:geom-destroy id)))


(defun bind-geom (geom rigid-body)
    (ode:geom-set-body (id-of geom) (id-of rigid-body)))

;;;
;;;
;;;
(defclass sphere-geom (geom) ())


(defun make-sphere-geom (system radius)
  (make-instance 'sphere-geom
                 :system system
                 :id (ode:create-sphere (space-of (universe)) radius)))


;;;
;;;
;;;
(defclass box-geom (geom) ())


(defun make-box-geom (system x y z)
  (make-instance 'box-geom
                 :system system
                 :id (ode:create-box (space-of (universe)) x y z)))



;;;
;;;
;;;
(defclass plane-geom (geom) ())


(defun make-plane-geom (system a b c &optional (z #f0))
  (make-instance 'plane-geom :system system
                 :id (ode:create-plane (space-of (universe)) a b c z)))


;;;
;;;
;;;
(defclass capped-cylinder-geom (geom) ())


(defun make-capped-cylinder-geom (system radius length)
  (make-instance 'capped-cylinder-geom
                 :system system
                 :id (ode:create-cylinder (space-of (universe)) radius length)))


;;;
;;;
;;;
(defclass ray-geom (geom) ())


(defun make-ray-geom (system length)
  (make-instance 'ray-geom :system system :id (ode:create-ray (space-of (universe)) length)))
