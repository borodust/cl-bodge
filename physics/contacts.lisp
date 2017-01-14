(in-package :cl-bodge.physics)


(defstruct (contact
             (:constructor make-contact (geom)))
  (geom nil :read-only t))


(defun contact-position (info)
  (c-let ((geom %ode:contact-geom :from (contact-geom info)))
    (vec3 (geom :pos 0) (geom :pos 1) (geom :pos 2))))


(defun contact-normal (info)
  (c-let ((geom %ode:contact-geom :from (contact-geom info)))
    (vec3 (geom :normal 0) (geom :normal 1) (geom :normal 2))))


(defun contact-depth (info)
  (c-ref (contact-geom info) %ode:contact-geom :depth))


(defun fill-contact-geom (contact-geom info)
  (memcpy (ptr contact-geom) (ptr (contact-geom info)) :type '%ode:contact-geom))


(defun fill-contact (contact info)
  (setf (%ode:contact.surface.mode contact) (mask 'contact-flags :approx0 :bounce)
        (%ode:contact.surface.mu contact) +infinity+
        (%ode:contact.surface.bounce contact) 1.0)
  (fill-contact-geom (%ode:contact.geom contact) info)
  contact)
