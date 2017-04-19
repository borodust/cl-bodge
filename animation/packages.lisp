(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.animation
  (:nicknames :ge.ani)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics)
  (:export make-keyframe
           make-keyframe-sequence
           transform-at
           make-keyframe-animation
           frame-at
           frame-key-of
           frame-transform-of
           make-animation-channel
           current-frame-of
           play-animation
           keyframed))
