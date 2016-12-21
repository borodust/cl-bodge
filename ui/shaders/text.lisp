(in-package :cl-bodge.interactions)


(define-shading-program text-shading
  :vertex-shader "v_text.glsl"
  :geometry-shader "g_text.glsl"
  :fragment-shader "f_text.glsl")
