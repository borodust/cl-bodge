(in-package :cl-bodge.resources)


(define-shader-library skeleton-animation
    :name "skeleton"
    :header "skeleton.h"
    :source "skeleton.glsl"
    :uniforms ("bones"))
