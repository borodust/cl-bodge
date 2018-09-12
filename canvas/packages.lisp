(bodge-util:define-package :cl-bodge.canvas
    (:nicknames :ge.vg)
  (:use :cl :cl-bodge.engine :bodge-util :cl-bodge.graphics)
  (:reexport-from :bodge-canvas
                  #:with-retained-canvas

                  #:path
                  #:fill-path
                  #:stroke-path
                  #:stroke-paint
                  #:fill-paint

                  #:scissors
                  #:move-to
                  #:line-to
                  #:bezier-to
                  #:rounded-rect
                  #:circle
                  #:ellipse
                  #:arc
                  #:text

                  #:apply-scissors
                  #:draw-line
                  #:draw-curve
                  #:draw-rect
                  #:draw-circle
                  #:draw-ellipse
                  #:draw-arc
                  #:draw-polygon
                  #:draw-polyline

                  #:translate-canvas
                  #:rotate-canvas
                  #:skew-canvas
                  #:scale-canvas
                  #:reset-canvas-transform

                  #:draw-text
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

           #:canvas-font-metrics
           #:canvas-text-bounds
           #:canvas-text-advance))
