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

           make-shader-pipeline

           make-array-buffer
           update-array-buffer
           make-index-buffer

           make-2d-texture
           make-empty-2d-texture
           texture-dimensions

           defsstruct

           make-framebuffer

           clear-rendering-output
           render

           preserving-state
           reset-state))
