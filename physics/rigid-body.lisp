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


(defmethod (setf position-of) ((value vec3) (this rigid-body))
  (declare (type vec3 value))
  (%ode:body-set-position (handle-value-of this)
                          (ode-real (vref value 0))
                          (ode-real (vref value 1))
                          (ode-real (vref value 2))))


(defmethod position-of ((this rigid-body))
  (ode->vec3 (%ode:body-get-position (handle-value-of this))))


(defmethod (setf mass-of) (value (this rigid-body))
  (declare (type mass value))
  (%ode:body-set-mass (handle-value-of this) (value-of value)))


(defmethod rotation-of ((this rigid-body))
  (ode->mat3 (%ode:body-get-rotation (handle-value-of this))))


(defmethod (setf rotation-of) ((value mat3) (this rigid-body))
  (with-ode-mat3 (m3 value)
    (%ode:body-set-rotation (handle-value-of this) m3)))


(defun apply-force (rigid-body vec3)
  (%ode:body-add-force (handle-value-of rigid-body)
                       (ode-real (x vec3))
                       (ode-real (y vec3))
                       (ode-real (z vec3))))


(defmethod transform-of ((this rigid-body))
  (c-let ((pos %ode:real :ptr (%ode:body-get-position (handle-value-of this)))
          (rot %ode:real :ptr (%ode:body-get-rotation (handle-value-of this))))
    (mat4 (rot 0) (rot 1) (rot 2)  (pos 0)
          (rot 4) (rot 5) (rot 6)  (pos 1)
          (rot 8) (rot 9) (rot 10) (pos 2)
          0.0     0.0     0.0      1.0)))
