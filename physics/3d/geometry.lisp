(cl:in-package :cl-bodge.physics)


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


(defmethod position-of ((this volume-geom))
  (ode->vec3 (%ode:geom-get-position (handle-value-of this))))


(defmethod (setf position-of) ((value vec3) (this volume-geom))
  (%ode:geom-set-position (handle-value-of this)
                          (ode-real (x value))
                          (ode-real (y value))
                          (ode-real (z value))))


(defmethod rotation-of ((this volume-geom))
  (ode->mat3 (%ode:geom-get-rotation (handle-value-of this))))


(defmethod (setf rotation-of) ((value mat3) (this volume-geom))
  (with-mat3-ptr (m3 value)
    (%ode:geom-set-rotation (handle-value-of this) m3)))


(defmethod transform-of ((this volume-geom))
  (ode-transform (%ode:geom-get-rotation (handle-value-of this))
                 (%ode:geom-get-position (handle-value-of this))))
;;;
;;;
;;;
(defclass sphere-geom (volume-geom) ())


(defmethod initialize-instance ((this sphere-geom) &rest args
                                &key (radius (error ":radius missing")))
  (apply #'call-next-method
         this
         :handle (make-geom-handle (%ode:create-sphere (space-of (universe)) (ode-real radius)))
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
                                                    (ode-real (x dimensions))
                                                    (ode-real (y dimensions))
                                                    (ode-real (z dimensions))))
         args))



;;;
;;;
;;;
(defclass cylinder-geom (volume-geom) ())


(defmethod initialize-instance ((this cylinder-geom) &rest args
                                &key (length (error ":length missing"))
                                  (radius (error ":radius missing")))
  (apply #'call-next-method
         this
         :handle (make-geom-handle (%ode:create-cylinder (space-of (universe))
                                                         (ode-real radius)
                                                         (ode-real length)))
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
                                     (ode-real (x normal))
                                     (ode-real (y normal))
                                     (ode-real (z normal))
                                     (ode-real offset)))
         args))

;;;
;;;
;;;
(defclass ray-geom (geom)
  ((position :reader position-of)
   (direction :reader direction-of)))


(definline set-ray (ray position direction)
  (%ode:geom-ray-set ray
                     (ode-real (x position))
                     (ode-real (y position))
                     (ode-real (z position))
                     (ode-real (x direction))
                     (ode-real (y direction))
                     (ode-real (z direction))))


(defmethod initialize-instance ((this ray-geom) &rest args
                                &key (position (vec3 0.0 0.0 0.0))
                                  (direction (error ":direction missing"))
                                  (length (error ":length missing")))
  (with-slots ((pos position) (dir direction)) this
    (let ((ode-ray (%ode:create-ray (space-of (universe)) (ode-real length))))
      (setf pos position
            dir direction)
      (set-ray ode-ray position direction)
      (apply #'call-next-method
             this
             :handle (make-geom-handle ode-ray)
             args))))


(defmethod (setf position-of) ((position vec3) (this ray-geom))
  (with-slots (direction (pos position)) this
    (setf pos position)
    (set-ray (handle-value-of this) position direction)))


(defmethod (setf direction-of) ((direction vec3) (this ray-geom))
  (with-slots (position (dir direction)) this
    (setf dir direction)
    (set-ray (handle-value-of this) position direction)))
