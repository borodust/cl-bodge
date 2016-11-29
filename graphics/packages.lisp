(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.graphics
  (:nicknames :ge.gx)
  (:use :cl :cl-bodge.engine :cl-bodge.host :cl-bodge.utils
        :cl-bodge.event :cl-bodge.graphics.resources)
  (:export graphics-system
           in-wireframe-mode

           render

           make-vertex-array

           attach-gpu-buffer
           make-array-buffer
           make-index-buffer

           primitive
           make-mesh
           make-patch-mesh

           compile-shader
           make-shading-program
           make-separable-shading-program
           link-separable-shading-program
           build-separable-shading-program

           use-shading-program
           with-using-shading-program
           program-uniform-variable

           with-bound-texture
           with-texture-unit
           make-2d-texture
           wrap-mode-of

           make-shading-pipeline
           use-shading-program-stages
           with-bound-shading-pipeline))
