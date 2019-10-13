(cl:in-package :cl-bodge.physics.ode)


(define-constant +precision+ 0f0)


(defclass ode-object (foreign-object) ())


(defgeneric direction-of (object))
(defgeneric (setf direction-of) (value object))


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
  (c-let ((vec %ode:real :from pointer))
    (vec3 (vec 0) (vec 1) (vec 2))))


(defun vec3->ode (vec3 pointer)
  (c-val ((pointer %ode:vector3))
    (setf (pointer 0) (ode-real (x vec3))
          (pointer 1) (ode-real (y vec3))
          (pointer 2) (ode-real (z vec3))))
  pointer)


(defun ode->mat3 (pointer)
  (c-let ((mat %ode:real :from pointer))
    (macrolet ((init ()
                 `(mat3 ,@(loop for i from 0 below 3
                                append (loop for j from 0 below 3 collect
                                             `(mat ,(+ (* i 4) j)))))))
      (init))))


(defun copy-mat3 (ptr mat3)
  (c-let ((m3 %ode:real :from ptr))
    (macrolet ((init ()
                 `(progn
                    ,@(loop for i from 0 below 3
                            append (loop for j from 0 below 3 collect
                                         `(setf (m3 ,(+ (* j 4) i))
                                                (ode-real (mref mat3 ,i ,j))))))))
      (init)
      ptr)))


(defmacro with-mat3-ptr ((mat-ptr bodge-mat3) &body body)
  (with-gensyms (mat)
    `(c-with ((,mat %ode:real :count (* 4 3) :clear t))
       (let ((,mat-ptr (,mat &)))
         (copy-mat3 ,mat-ptr ,bodge-mat3)
         ,@body))))


(defun ode-transform (rotation-ptr position-ptr)
  (c-let ((pos %ode:real :from position-ptr)
          (rot %ode:real :from rotation-ptr))
    (mat4 (rot 0) (rot 1) (rot 2)  (pos 0)
          (rot 4) (rot 5) (rot 6)  (pos 1)
          (rot 8) (rot 9) (rot 10) (pos 2)
          0.0     0.0     0.0      1.0)))
