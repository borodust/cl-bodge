(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.canvas
  (:nicknames :ge.vg)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics :autowrap :plus-c)
  (:export make-canvas
           update-canvas-size
           begin-canvas
           end-canvas
           with-canvas
           push-canvas
           pop-canvas

           path
           fill-path
           stroke-path
           stroke-paint
           fill-paint
           make-image-paint
           move-to
           scissors
           draw-line
           draw-curve
           draw-rect
           draw-circle
           draw-ellipse
           draw-arc
           draw-polygon
           draw-polyline

           translate-canvas
           rotate-canvas
           skew-canvas
           scale-canvas
           reset-canvas-transform))
