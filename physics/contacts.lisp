(in-package :cl-bodge.physics)


(defstruct (contact-surface
             (:constructor make-contact-surface))
  (friction +infinity+)
  (bounciness 0.0))


(defstruct (contact
             (:constructor make-contact (geom)))
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


(defun fill-contact-geom (contact-geom info)
  (memcpy (ptr contact-geom) (ptr (contact-geom info)) :type '%ode:contact-geom))


(defun fill-contact (contact info)
  (c-val ((contact %ode:contact))
    (setf (contact :surface :mode) (mask 'contact-flags :approx0 :bounce)
          (contact :surface :mu) (ode-real (surface-friction info))
          (contact :surface :bounce) (ode-real (surface-bounciness info)))
    (fill-contact-geom (contact :geom &) info))
  contact)
