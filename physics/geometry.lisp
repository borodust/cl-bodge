(in-package :cl-bodge.physics)


(defclass geom (ode-object)
  ())


(defmethod initialize-instance :around ((this geom) &key)
  (call-next-method)
  (%register-geom (universe) this))


(define-destructor geom ((id id-of) (sys system-of))
  (-> (sys :priority :low)
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


(define-system-function make-sphere-geom physics-system (radius &key (system *system*))
  (make-instance 'sphere-geom
                 :system system
                 :id (%ode:create-sphere (space-of (universe)) radius)))


;;;
;;;
;;;
(defclass box-geom (volume-geom) ())


(define-system-function make-box-geom physics-system (x y z &key (system *system*))
  (make-instance 'box-geom
                 :system system
                 :id (%ode:create-box (space-of (universe)) x y z)))



;;;
;;;
;;;
(defclass plane-geom (geom) ())


(define-system-function make-plane-geom physics-system (a b c z &key (system *system*))
  (make-instance 'plane-geom :system system
                 :id (%ode:create-plane (space-of (universe)) a b c z)))


;;;
;;;
;;;
(defclass capped-cylinder-geom (volume-geom) ())


(define-system-function make-capped-cylinder-geom physics-system
    (radius length &key (system *system*))
  (make-instance 'capped-cylinder-geom
                 :system system
                 :id (%ode:create-cylinder (space-of (universe)) radius length)))


;;;
;;;
;;;
(defclass ray-geom (geom) ())


(define-system-function make-ray-geom physics-system (length &key (system *system*))
  (make-instance 'ray-geom :system system :id (%ode:create-ray (space-of (universe)) length)))
