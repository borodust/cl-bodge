(in-package :cl-bodge.text)


(define-shading-program text-shading
  :vertex-shader "text.v.glsl"
  :geometry-shader "text.g.glsl"
  :fragment-shader "text.f.glsl")
