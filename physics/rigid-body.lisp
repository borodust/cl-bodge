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


;; fixme memory sink
(defmethod position-of ((this rigid-body))
  (let ((ode-pos (%ode:body-get-position (handle-value-of this))))
    (flet ((el (idx) (f (c-ref ode-pos %ode:real idx))))
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
                                    `(f (c-ref m3 %ode:real ,(+ (* j 4) i))))))))
      (init))))


(defmethod (setf rotation-of) ((value mat3) (this rigid-body))
  (c-with ((m3 %ode:real :calloc t :count (* 4 3)))
    (macrolet ((init ()
                 `(progn
                    ,@(loop for i from 0 below 3 append
                           (loop for j from 0 below 3 collect
                                `(setf (m3 ,(+ (* j 4) i))
                                       (ode-real (mref value ,i ,j))))))))
      (init)
      (%ode:body-set-rotation (handle-value-of this) (m3 &)))))


(defun apply-force (rigid-body vec3)
  (%ode:body-add-force (handle-value-of rigid-body)
                       (ode-real (vref vec3 0))
                       (ode-real (vref vec3 1))
                       (ode-real (vref vec3 2))))
