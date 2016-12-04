(in-package :cl-bodge.physics)


(defclass rigid-body (ode-object) ())


(define-destructor rigid-body ((id id-of) (system system-of))
  (-> (system :priority :low)
    (%ode:body-destroy id)))


(defgeneric position-of (this))
(defgeneric (setf position-of) (value this))
(defgeneric rotation-of (this))
(defgeneric linear-velocity-of (this))
(defgeneric angular-velocity-of (this))
(defgeneric mass-of (this))
(defgeneric (setf mass-of) (value this))


(define-system-function make-rigid-body physics-system (&key (system *system*))
  (make-instance 'rigid-body
                 :system system
                 :id (%ode:body-create (world-of (universe)))))


(defmethod enable ((this rigid-body))
  (%ode:body-enable (id-of this)))


(defmethod disable ((this rigid-body))
  (%ode:body-disable (id-of this)))


(defmethod enabledp ((this rigid-body))
  (> (%ode:body-is-enabled (id-of this)) 0))


(defmethod (setf position-of) (value (this rigid-body))
  (declare (type vec3 value))
  (%ode:body-set-position (id-of this) (vref value 0) (vref value 1) (vref value 2)))


;; fixme memory sink
(defmethod position-of ((this rigid-body))
  (let ((ode-pos (%ode:body-get-position (id-of this))))
    (flet ((el (idx) #f(c-aref ode-pos idx '%ode:real)))
      (vec3 (el 0) (el 1) (el 2)))))


(defmethod (setf mass-of) (value (this rigid-body))
  (declare (type mass value))
  (%ode:body-set-mass (id-of this) (value-of value)))


;; fixme memory sink
(defmethod rotation-of ((this rigid-body))
  (let ((m3 (%ode:body-get-rotation (id-of this))))
    (macrolet ((init ()
                 `(mat3 ,@(loop for i from 0 below 3 append
                               (loop for j from 0 below 3 collect
                                    `(float (c-aref m3 ,(+ (* j 4) i) '%ode:real)))))))
      (init))))


(defun apply-force (rigid-body vec3)
  (%ode:body-add-force (id-of rigid-body) (vref vec3 0) (vref vec3 1) (vref vec3 2)))
