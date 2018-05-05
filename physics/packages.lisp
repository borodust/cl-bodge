(ge.util:define-package :cl-bodge.physics
  (:use :cl-bodge.engine :cl :cl-bodge.utils :cl-bodge.physics.backend)
  (:nicknames :ge.phy)
  (:export physics-system
           physics

           make-universe
           observe-universe
           define-universe-material
           gravity

           make-rigid-body
           infuse-circle-mass
           apply-force
           apply-torque
           body-force
           body-torque
           body-position
           body-rotation
           body-linear-velocity
           body-angular-velocity

           shape-substance
           shape-body
           make-plane-shape
           make-sphere-shape
           make-cube-shape

           make-segment-shape
           make-polygon-shape
           make-polyline-shape
           make-box-shape
           make-circle-shape))
