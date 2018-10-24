(bodge-util:define-package :cl-bodge.physics
  (:use :cl-bodge.engine :cl :bodge-util :cl-bodge.physics.backend)
  (:nicknames :ge.phy)
  (:export physics-system
           physics

           make-universe
           observe-universe
           define-universe-material
           gravity

           make-rigid-body
           infuse-circle-mass
           infuse-box-mass
           infuse-sphere-mass
           infuse-cuboid-mass
           make-kinematic-body
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
           make-cuboid-shape

           make-segment-shape
           make-polygon-shape
           make-polyline-shape
           make-box-shape
           make-circle-shape

           collision-friction
           collision-elasticity
           collision-surface-velocity

           make-damped-string-constraint))
