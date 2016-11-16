(in-package :cl-bodge.physics)


(defclass geom (ode-object)
  ())


(defmethod initialize-instance :around ((this geom) &key)
  (call-next-method)
  (%register-geom (universe) this))


(define-destructor geom ((id id-of) (sys system-of))
  (-> sys
    (%ode:geom-destroy id)))


(defmethod enable ((this geom))
  (%ode:geom-enable (id-of this)))


(defmethod disable ((this geom))
  (%ode:geom-disable (id-of this)))


(defmethod enabledp ((this geom))
  (> (%ode:geom-is-enabled (id-of this)) 0))

;;;
;;;
;;;
(defclass volume-geom (geom) ())


(defgeneric bind-geom (geom rigid-body)
  (:method ((this volume-geom) rigid-body)
    (%ode:geom-set-body (id-of this) (id-of rigid-body))))

;;;
;;;
;;;
(defclass sphere-geom (volume-geom) ())


(defun make-sphere-geom (system radius)
  (make-instance 'sphere-geom
                 :system system
                 :id (%ode:create-sphere (space-of (universe)) radius)))


;;;
;;;
;;;
(defclass box-geom (volume-geom) ())


(defun make-box-geom (system x y z)
  (make-instance 'box-geom
                 :system system
                 :id (%ode:create-box (space-of (universe)) x y z)))



;;;
;;;
;;;
(defclass plane-geom (geom) ())


(defun make-plane-geom (system a b c &optional (z #f0))
  (make-instance 'plane-geom :system system
                 :id (%ode:create-plane (space-of (universe)) a b c z)))


;;;
;;;
;;;
(defclass capped-cylinder-geom (volume-geom) ())


(defun make-capped-cylinder-geom (system radius length)
  (make-instance 'capped-cylinder-geom
                 :system system
                 :id (%ode:create-cylinder (space-of (universe)) radius length)))


;;;
;;;
;;;
(defclass ray-geom (geom) ())


(defun make-ray-geom (system length)
  (make-instance 'ray-geom :system system :id (%ode:create-ray (space-of (universe)) length)))
