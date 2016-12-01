(in-package :cl-bodge.assets)


(define-shader-library lighting-library
    :descriptor-path (:cl-bodge/assets shaders "lighting")
    :name "lighting"
    :header "lighting.h"
    :source "lighting.glsl"
    :uniforms ("dLight.ambient"
               "dLight.diffuse"
               "dLight.direction"))
