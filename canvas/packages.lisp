(ge.util:define-package :cl-bodge.canvas
  (:nicknames :ge.vg)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics :claw)
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
           make-default-font
           with-font
           draw-text
           canvas-text-bounds
           canvas-text-advance
           canvas-font-metrics
           canvas-font-line-height
           canvas-font-ascender
           canvas-font-descender))
