(ge.util:define-package :cl-bodge.graphics.state
  (:nicknames :ge.gx.state)
  (:export enable
           disable))


(ge.util:define-package :cl-bodge.graphics
  (:nicknames :ge.gx)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.host)
  (:import-from :cl-bodge.resources
                #:image-height
                #:image-width
                #:image-pixel-format
                #:image->foreign-array)
  (:export graphics-system
           graphics
           graphics-context
           graphics-context-assembly-flow
           for-graphics
           reset-viewport

           defshader
           defpipeline
           defamalgam

           make-shader-pipeline

           make-array-buffer
           update-array-buffer
           make-index-buffer

           make-shader-structure

           make-2d-texture
           make-empty-2d-texture
           make-empty-depth-texture
           texture-dimensions
           texture-mipmap-level

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
           finish-rendering-output
           render
           shader-source

           preserving-state
           reset-state

           make-ping-pong-pair
           ping-pong-swap
           with-ping-pong-front
           with-ping-pong-back))
