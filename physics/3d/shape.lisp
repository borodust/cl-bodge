(cl:in-package :cl-bodge.physics.ode)


(defmethod simulation-engine-shape-substance ((engine ode-engine) shape)
  (substance-of shape))


(defmethod simulation-engine-shape-body ((engine ode-engine) shape)
  (body-of shape))


(defmethod simulation-engine-destroy-shape ((engine ode-engine) shape)
  (dispose shape))


(defmethod simulation-engine-make-sphere-shape ((engine ode-engine) universe radius
                                                &key body substance)
  (let ((geom (make-instance 'sphere-geom :universe universe
                                          :radius radius
                                          :body body
                                          :substance substance)))
    (when body
      (bind-geom geom body))
    geom))


(defmethod simulation-engine-make-cuboid-shape ((engine ode-engine) universe width height depth
                                                &key body substance)
  (let ((geom (make-instance 'box-geom :universe universe
                                       :dimensions (vec3 width height depth)
                                       :body body
                                       :substance substance)))
    (when body
      (bind-geom geom body))
    geom))
