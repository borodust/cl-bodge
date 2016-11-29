(in-package :cl-bodge.asdf)



(defpackage :cl-bodge.animation
  (:nicknames :ge.ani)
  (:use :cl :cl-bodge.utils :cl-bodge.math)
  (:export make-keyframe
           make-keyframe-sequence
           transform-at
           make-keyframe-animation
           frame-at
           frame-transform-of
           start-animation
           reset-animation
           keyframed))
