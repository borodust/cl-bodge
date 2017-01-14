(in-package :cl-bodge.physics)


(defclass ode-object (foreign-object) ())


(defgeneric direction-of (object))
(defgeneric (setf direction-of) (value object))


(defgeneric collide (this-geom that-geom)
  (:method (this-geom that-geom) t))


(defgeneric filter-contacts (contacts this-geom that-geom)
  (:method (contacts this-geom that-geom) contacts))


;;;
;;;
;;;
(defclass collidable () ())


(defgeneric collidablep (obj)
  (:method (obj) nil))


(defmethod collidablep ((this collidable))
  t)


(defmethod collide ((this collidable) (that collidable))
  nil)
