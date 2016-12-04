(in-package :cl-bodge.assets)


(define-shader-library skinning-library
    :name "skinning"
    :header "skinning.h"
    :source "skinning.glsl"
    :uniforms ("bones"))
