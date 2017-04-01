(in-package :cl-bodge.library.shading)


(define-shading-program banner
  :vertex-shader "banner.v.glsl"
  :fragment-shader "banner.f.glsl")
