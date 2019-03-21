(ge.util:define-package :cl-bodge.canvas
  (:nicknames :ge.vg)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics)
  (:reexport-from :bodge-canvas
                  #:with-retained-canvas

                  #:path
                  #:fill-path
                  #:stroke-path
                  #:stroke-width

                  #:scissors
                  #:move-to
                  #:line-to
                  #:bezier-to
                  #:rounded-rect
                  #:circle
                  #:ellipse
                  #:arc
                  #:text
                  #:wind-path
                  #:line-cap
                  #:line-join

                  #:apply-scissors

                  #:translate-canvas
                  #:rotate-canvas
                  #:skew-canvas
                  #:scale-canvas
                  #:reset-canvas-transform

                  #:make-font
                  #:make-default-font
                  #:with-font

                  #:canvas-font-line-height
                  #:canvas-font-ascender
                  #:canvas-font-descender)
  (:export #:defcanvas
           #:make-canvas
           #:canvas-width
           #:canvas-height

           #:update-canvas-size
           #:update-canvas-pixel-ratio

           #:make-image-paint
           #:image-paint-width
           #:image-paint-height

           #:fill-paint
           #:stroke-paint

           #:draw-line
           #:draw-curve
           #:draw-rect
           #:draw-circle
           #:draw-ellipse
           #:draw-arc
           #:draw-polygon
           #:draw-polyline
           #:draw-text
           #:draw-image

           #:register-font-face
           #:canvas-font-metrics
           #:canvas-text-bounds
           #:canvas-text-advance))
