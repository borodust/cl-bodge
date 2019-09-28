(cl:in-package :cl-bodge.physics.ode)


(defstruct (contact-surface
             (:constructor make-contact-surface))
  (friction 0.05d0)
  (bounciness 0.4d0)
  (velocity 0d0))


(defstruct (contact
             (:constructor %make-contact (geom)))
  (geom nil :read-only t)
  (surface (make-contact-surface)))


(defun contact-position (info)
  (c-let ((geom %ode:contact-geom :from (contact-geom info)))
    (vec3 (geom :pos 0) (geom :pos 1) (geom :pos 2))))


(defun contact-normal (info)
  (c-let ((geom %ode:contact-geom :from (contact-geom info)))
    (vec3 (geom :normal 0) (geom :normal 1) (geom :normal 2))))


(defun contact-depth (info)
  (c-ref (contact-geom info) %ode:contact-geom :depth))


(defun surface-friction (info)
  (contact-surface-friction (contact-surface info)))


(defun (setf surface-friction) (value info)
  (setf (contact-surface-friction (contact-surface info)) value))


(defun surface-bounciness (info)
  (contact-surface-bounciness (contact-surface info)))


(defun (setf surface-bounciness) (value info)
  (setf (contact-surface-bounciness (contact-surface info)) value))


(defun surface-velocity (info)
  (contact-surface-velocity (contact-surface info)))


(defun (setf surface-velocity) (value info)
  (setf (contact-surface-velocity (contact-surface info)) value))


(defun make-contact (geom friction elasticity surface-velocity)
  (let ((contact (%make-contact geom)))
    (when friction
      (setf (surface-friction contact) friction))
    (when elasticity
      (setf (surface-bounciness contact) elasticity))
    (when surface-velocity
      (setf (surface-velocity contact) surface-velocity))
    contact))


(defun fill-contact-geom (contact-geom info)
  (%libc.es:memcpy contact-geom
                   (contact-geom info)
                   (cffi:foreign-type-size '%ode:contact-geom)))


(defun fill-contact (contact info)
  (c-val ((contact %ode:contact))
    (setf (contact :surface :mode) (cffi:foreign-bitfield-value 'ode:contact-flags
                                                                '(:approx1
                                                                  :bounce
                                                                  :motion1))
          (contact :surface :mu) (ode-real (surface-friction info))
          (contact :surface :bounce) (ode-real (surface-bounciness info))
          (contact :surface :motion1) (surface-velocity info))
    (fill-contact-geom (contact :geom &) info))
  contact)
