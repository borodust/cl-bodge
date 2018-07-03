(ge.util:define-package :cl-bodge.shading
  (:nicknames :ge.shad)
  (:use :cl :cl-bodge.engine :cl-bodge.graphics :cl-bodge.utils)
  (:export phong-shader

           phong-material
           phong-material-specular-scale
           phong-material-shininess
           phong-material-roughness
           phong-material-albedo

           phong-point-light
           phong-point-light-position
           phong-point-light-color
           phong-point-light-ambient
           phong-point-light-falloff
           phong-point-light-radius

           banner-pipeline
           banner-position
           banner-tex-coord
           banner-mvp
           banner-texture))
