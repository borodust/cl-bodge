(ge.util:define-package :cl-bodge.shading
  (:nicknames :ge.shad)
  (:use :cl :cl-bodge.engine :cl-bodge.graphics :cl-bodge.utils)
  (:export phong-shader
           phong-material
           phong-point-light))
