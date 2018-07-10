(ge.util:define-package :cl-bodge.graphics.state
  (:nicknames :gx.state)
  (:export enable
           disable))


(ge.util:define-package :cl-bodge.graphics
  (:nicknames :ge.gx)
  (:use :cl :cl-bodge.engine :cl-bodge.host :cl-bodge.utils)
  (:export graphics-system
           graphics
           for-graphics

           defshader
           defpipeline
           defsstruct

           make-shader-pipeline

           make-array-buffer
           update-array-buffer
           make-index-buffer

           make-2d-texture
           make-empty-2d-texture
           make-empty-depth-texture
           texture-dimensions

           make-cubemap-texture
           make-empty-cubemap-texture
           make-empty-depth-cubemap-texture
           cubemap-positive-x-layer
           cubemap-positive-y-layer
           cubemap-positive-z-layer
           cubemap-negative-x-layer
           cubemap-negative-y-layer
           cubemap-negative-z-layer
           do-cubemap-layers

           make-framebuffer
           configure-framebuffer

           clear-rendering-output
           render
           shader-source

           preserving-state
           reset-state))
