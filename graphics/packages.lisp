(ge.util:define-package :cl-bodge.graphics.state
  (:nicknames :gx.state)
  (:export enable
           disable))


(ge.util:define-package :cl-bodge.graphics
  (:nicknames :ge.gx)
  (:use :cl :cl-bodge.engine :cl-bodge.host :cl-bodge.utils)
  (:export graphics-system
           graphics
           rendering
           in-wireframe-mode
           preserving-state
           reset-state

           render

           make-vertex-array

           attach-array-buffer
           update-array-buffer
           make-array-buffer
           make-index-buffer

           primitive
           make-mesh
           submesh
           render-mesh
           make-patch-mesh

           shader-type-of
           shader-name-of
           shader-text-of
           compile-shader
           shading-program
           make-shading-program
           program-separable
           uniforms-of
           uniform-name

           use-shading-program
           with-active-shading-program
           program-uniform-variable

           make-directional-light-source
           apply-light-source

           dimensions-of
           with-bound-texture
           with-texture-unit
           make-2d-texture
           wrap-mode-of

           make-blank-image

           make-shading-pipeline
           use-shading-program-stages
           with-bound-shading-pipeline

           make-framebuffer
           with-bound-framebuffer
           attach-color-buffer
           attach-depth-buffer
           attach-stencil-buffer
           attach-depth-stencil-buffer
           detach-color-buffer
           detach-depth-buffer
           detach-stencil-buffer
           detach-depth-stencil-buffer
           with-complete-framebuffer

           make-renderbuffer))
