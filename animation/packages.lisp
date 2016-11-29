(in-package :cl-bodge.asdf)



(defpackage :cl-bodge.animation
  (:nicknames :ge.ani)
  (:use :cl-bodge.utils :cl-bodge.math
        :cl)
  (:export make-keyframe
           make-keyframe-sequence
           transform-at
           make-keyframe-animation
           frame-at
           frame-transform-of
           start-animation
           reset-animation
           keyframed))
