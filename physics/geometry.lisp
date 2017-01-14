(in-package :cl-bodge.physics)


(defhandle geom-handle
    :closeform (%ode:geom-destroy *handle-value*))


(defclass geom (ode-object) ())


(defmethod initialize-instance :around ((this geom) &key)
  (call-next-method)
  (%register-geom (universe) this))


(defmethod enable ((this geom))
  (%ode:geom-enable (handle-value-of this)))


(defmethod disable ((this geom))
  (%ode:geom-disable (handle-value-of this)))


(defmethod enabledp ((this geom))
  (> (%ode:geom-is-enabled (handle-value-of this)) 0))

;;;
;;;
;;;
(defclass volume-geom (geom) ())


(defun bind-geom (geom rigid-body)
  (%ode:geom-set-body (handle-value-of geom) (handle-value-of rigid-body)))

;;;
;;;
;;;
(defclass sphere-geom (volume-geom) ())


(defmethod initialize-instance ((this sphere-geom) &rest args
                                &key (radius (error ":radius missing")))
  (apply #'call-next-method
         this
         :handle (make-geom-handle (%ode:create-sphere (space-of (universe)) radius))
         args))

;;;
;;;
;;;
(defclass box-geom (volume-geom) ())


(defmethod initialize-instance ((this box-geom) &rest args
                                &key (dimensions (error ":dimensions missing")))
  (apply #'call-next-method
         this
         :handle (make-geom-handle (%ode:create-box (space-of (universe))
                                                    (x dimensions)
                                                    (y dimensions)
                                                    (z dimensions)))
         args))



;;;
;;;
;;;
(defclass plane-geom (geom) ())


(defmethod initialize-instance ((this plane-geom) &rest args
                                &key (normal (error ":normal missing"))
                                  (offset 0.0))
  (apply #'call-next-method
         this
         :handle (make-geom-handle
                  (%ode:create-plane (space-of (universe))
                                     (x normal) (y normal) (z normal) offset))
         args))

;;;
;;;
;;;
(defclass ray-geom (geom)
  ((position :reader position-of)
   (direction :reader direction-of)))


(defmethod initialize-instance ((this ray-geom) &rest args
                                &key (position (vec3 0.0 0.0 0.0))
                                  (direction (error ":direction missing"))
                                  (length (error ":length missing")))
  (with-slots ((pos position) (dir direction)) this
    (let ((ode-ray (%ode:create-ray (space-of (universe)) length)))
      (setf pos position
            dir direction)
      (%ode:geom-ray-set ode-ray
                         (x position) (y position) (z position)
                         (x direction) (y direction) (z direction))
      (apply #'call-next-method
             this
             :handle (make-geom-handle ode-ray)
             args))))


(defmethod (setf position-of) ((position vec3) (this ray-geom))
  (with-slots (direction) this
    (%ode:geom-ray-set (handle-value-of this)
                       (x position) (y position) (z position)
                       (x direction) (y direction) (z direction))))


(defmethod (setf direction-of) ((direction vec3) (this ray-geom))
  (with-slots (position) this
    (%ode:geom-ray-set (handle-value-of this)
                       (x position) (y position) (z position)
                       (x direction) (y direction) (z direction))))
