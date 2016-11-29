(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.physics
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.math :cl-bodge.memory
        :cl-bodge.concurrency :cl-bodge.utils
        :cl :local-time :bodge-ode :autowrap :plus-c)
  (:nicknames :ge.phx)
  (:export physics-system
           observe-universe
           gravity
           register-collision-callback
           register-contact-callback

           make-rigid-body
           position-of
           rotation-of
           linear-velocity-of
           angular-velocity-of
           mass-of
           apply-force

           make-ball-joint
           make-hinge-joint
           make-slider-joint
           make-universal-joint
           make-double-hinge-joint
           make-angular-motor-joint

           make-sphere-geom
           make-box-geom
           make-plane-geom
           make-capped-cylinder-geom
           make-ray-geom
           bind-geom

           make-box-mass
           make-sphere-mass))
