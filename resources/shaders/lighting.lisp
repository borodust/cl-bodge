(in-package :cl-bodge.resources)


(define-shader-library lighting-library
    :name "lighting"
    :header "lighting.h"
    :source "lighting.glsl"
    :uniforms ("dLight.ambient"
               "dLight.diffuse"
               "dLight.direction"))
