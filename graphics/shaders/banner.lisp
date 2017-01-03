(in-package :cl-bodge.graphics)


(define-shading-program banner
  :vertex-shader "banner.v.glsl"
  :fragment-shader "banner.f.glsl")
