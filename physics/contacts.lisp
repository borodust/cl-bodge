(in-package :cl-bodge.physics)


(defstruct (contact
             (:constructor make-contact (geom)))
  (geom nil :read-only t))


(defun contact-position (info)
  (c-let ((geom %ode:contact-geom :from (contact-geom info)))
    (vec3 (f (geom :pos 0)) (f (geom :pos 1)) (f (geom :pos 2)))))


(defun contact-normal (info)
  (c-let ((geom %ode:contact-geom :from (contact-geom info)))
    (vec3 (f (geom :normal 0)) (f (geom :normal 1)) (f (geom :normal 2)))))


(defun contact-depth (info)
  (f (c-ref (contact-geom info) %ode:contact-geom :depth)))


(defun fill-contact-geom (contact-geom info)
  (memcpy (ptr contact-geom) (ptr (contact-geom info)) :type '%ode:contact-geom))


(defun fill-contact (contact info)
  (setf (%ode:contact.surface.mode contact) (mask 'contact-flags :approx0 :bounce)
        (%ode:contact.surface.mu contact) +infinity+
        (%ode:contact.surface.bounce contact) (ode-real 1.0))
  (fill-contact-geom (%ode:contact.geom contact) info)
  contact)
