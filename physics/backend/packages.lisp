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
           #:simulation-engine-body-torque
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
           #:simulation-engine-destroy-shape

           #:simulation-engine-contact-friction
           #:simulation-engine-contact-elasticity
           #:simulation-engine-contact-surface-velocity
           #:simulation-engine-contact-normal
           #:simulation-engine-contact-point
           #:simulation-engine-contact-depth
           #:simulation-engine-contact-this-shape
           #:simulation-engine-contact-that-shape))
