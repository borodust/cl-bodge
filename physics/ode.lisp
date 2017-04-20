(in-package :cl-bodge.physics)


(define-constant +precision+ (if ode:+double-precision-p+ 0d0 0f0)
  :test #'=)

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


(defun copy-mat3 (pointer mat3)
  (c-let ((m3 %ode:real :ptr pointer))
    (macrolet ((init ()
                 `(progn
                    ,@(loop for i from 0 below 3 append
                           (loop for j from 0 below 3 collect
                                `(setf (m3 ,(+ (* j 4) i))
                                       (ode-real (mref mat3 ,i ,j))))))))
      (init)
      pointer)))


(defmacro with-ode-mat3 ((ode-mat bodge-mat3) &body body)
  `(c-with ((,ode-mat %ode:real :calloc t :count (* 4 3)))
     (copy-mat3 ,ode-mat ,bodge-mat3)
     ,@body))
