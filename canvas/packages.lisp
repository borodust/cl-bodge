(cl:defpackage :cl-bodge.canvas
  (:nicknames :ge.vg)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics :bodge-autowrap :bodge-plus-c)
  (:export make-canvas
           update-canvas-size
           begin-canvas
           end-canvas
           flush-canvas
           with-canvas
           push-canvas
           pop-canvas
           with-pushed-canvas

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
           reset-canvas-transform

           register-font-face
           make-font
           with-font
           draw-text))
