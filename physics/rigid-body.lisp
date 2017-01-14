(in-package :cl-bodge.physics)


(defhandle rigid-body-handle
    :initform (%ode:body-create (world-of (universe)))
    :closeform (%ode:body-destroy *handle-value*))


(defclass rigid-body (ode-object) ())


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
                 :handle (make-rigid-body-handle)))


(defmethod enable ((this rigid-body))
  (%ode:body-enable (handle-value-of this)))


(defmethod disable ((this rigid-body))
  (%ode:body-disable (handle-value-of this)))


(defmethod enabledp ((this rigid-body))
  (> (%ode:body-is-enabled (handle-value-of this)) 0))


(defmethod (setf position-of) (value (this rigid-body))
  (declare (type vec3 value))
  (%ode:body-set-position (handle-value-of this) (vref value 0) (vref value 1) (vref value 2)))


;; fixme memory sink
(defmethod position-of ((this rigid-body))
  (let ((ode-pos (%ode:body-get-position (handle-value-of this))))
    (flet ((el (idx) #f(c-ref ode-pos %ode:real idx)))
      (vec3 (el 0) (el 1) (el 2)))))


(defmethod (setf mass-of) (value (this rigid-body))
  (declare (type mass value))
  (%ode:body-set-mass (handle-value-of this) (value-of value)))


;; fixme memory sink
(defmethod rotation-of ((this rigid-body))
  (let ((m3 (%ode:body-get-rotation (handle-value-of this))))
    (macrolet ((init ()
                 `(mat3 ,@(loop for i from 0 below 3 append
                               (loop for j from 0 below 3 collect
                                    `(float (c-ref m3 %ode:real ,(+ (* j 4) i)) 0f0))))))
      (init))))


(defun apply-force (rigid-body vec3)
  (%ode:body-add-force (handle-value-of rigid-body) (vref vec3 0) (vref vec3 1) (vref vec3 2)))
