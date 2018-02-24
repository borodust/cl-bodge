(cl:in-package :cl-bodge.physics)


(define-constant +precision+ 0d0)


(defvar *contact-points-per-collision* 1)


(defclass ode-object (system-foreign-object) ())


(defgeneric direction-of (object))
(defgeneric (setf direction-of) (value object))


(defgeneric collide (this-geom that-geom)
  (:method (this-geom that-geom) t))


(defgeneric filter-contacts (contacts this-geom that-geom)
  (:method (contacts this-geom that-geom) contacts))


(defgeneric contacts-per-collision (this-geom that-geom)
  (:method (this that) *contact-points-per-collision*))



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


(definline ode-real (value)
  (float value +precision+))


(defun ode->vec3 (pointer)
  (c-let ((vec %ode:real :ptr pointer))
    (vec3 (vec 0) (vec 1) (vec 2))))


(defun ode->mat3 (pointer)
  (c-let ((mat %ode:real :ptr pointer))
    (macrolet ((init ()
                 `(mat3 ,@(loop for i from 0 below 3 append
                               (loop for j from 0 below 3 collect
                                    `(mat ,(+ (* j 4) i)))))))
      (init))))


(defun copy-mat3 (ptr mat3)
  (c-let ((m3 %ode:real :ptr ptr))
    (macrolet ((init ()
                 `(progn
                    ,@(loop for i from 0 below 3 append
                           (loop for j from 0 below 3 collect
                                `(setf (m3 ,(+ (* j 4) i))
                                       (ode-real (mref mat3 ,i ,j))))))))
      (init)
      ptr)))


(defmacro with-mat3-ptr ((mat-ptr bodge-mat3) &body body)
  `(with-calloc (,mat-ptr '%ode:real (* 4 3))
     (copy-mat3 ,mat-ptr ,bodge-mat3)
     ,@body))


(defun ode-transform (rotation-ptr position-ptr)
  (c-let ((pos %ode:real :ptr position-ptr)
          (rot %ode:real :ptr rotation-ptr))
    (mat4 (rot 0) (rot 1) (rot 2)  (pos 0)
          (rot 4) (rot 5) (rot 6)  (pos 1)
          (rot 8) (rot 9) (rot 10) (pos 2)
          0.0     0.0     0.0      1.0)))
