(in-package :cl-bodge.physics)


(defclass rigid-body (ode-object) ())


(define-destructor rigid-body ((id id-of) (system system-of))
  (-> system
    (ode:body-destroy id)))


(defgeneric position-of (this))
(defgeneric rotation-of (this))
(defgeneric linear-velocity-of (this))
(defgeneric angular-velocity-of (this))
(defgeneric mass-of (this))


(defun make-rigid-body (system)
  (make-instance 'rigid-body
                 :system system
                 :id (ode:body-create (world-of (universe)))))


(defmethod (setf position-of) (value (this rigid-body))
  (declare (type vec3 value))
  (ode:body-set-position (id-of this) (vref value 0) (vref value 1) (vref value 2)))


;; fixme memory sink
(defmethod position-of ((this rigid-body))
  (let ((ode-pos (ode:body-get-position (id-of this))))
    (flet ((el (idx) #f(cffi:mem-aref ode-pos 'ode:real idx)))
      (make-vec3* (el 0) (el 1) (el 2)))))


(defmethod (setf mass-of) (value (this rigid-body))
  (declare (type mass value))
  (ode:body-set-mass (id-of this) (value-of value)))


;; fixme memory sink
(defmethod rotation-of ((this rigid-body))
  (loop with rotation = (identity-mat4) and m3 = (ode:body-get-rotation (id-of this))
     for i from 0 below 3 do
       (loop for j from 0 below 3 do
            (setf (mref rotation i j) #f(cffi:mem-aref m3 'ode:real (+ (* i 4) j))))
     finally (return rotation)))
