(ge.util:define-package :cl-bodge.physics.backend
  (:use :cl)
  (:export #:register-simulation-engine
           #:list-simulation-engines
           #:find-simulation-engine-by-name
           #:simulation-engine-initialize
           #:simulation-engine-discard

           #:simulation-engine-make-universe
           #:simulation-engine-destroy-universe
           #:simulation-engine-gravity
           #:simulation-engine-observe-universe

           #:simulation-engine-make-mass-for-circle

           #:simulation-engine-make-rigid-body
           #:simulation-engine-destroy-rigid-body
           #:simulation-engine-apply-force
           #:simulation-engine-body-force
           #:simulation-engine-apply-torque
           #:simulation-engine-body-force
           #:simulation-engine-body-mass
           #:simulation-engine-body-position
           #:simulation-engine-body-linear-velocity
           #:simulation-engine-body-rotation
           #:simulation-engine-body-angular-velocity

           #:simulation-engine-shape-substance
           #:simulation-engine-shape-body
           #:simulation-engine-make-segment-shape
           #:simulation-engine-make-polyline-shape
           #:simulation-engine-make-polygon-shape
           #:simulation-engine-make-box-shape
           #:simulation-engine-make-circle-shape
           #:simulation-engine-destroy-shape))
