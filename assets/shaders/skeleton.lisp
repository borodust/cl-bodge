(in-package :cl-bodge.assets)


(define-shader-library skeleton-animation
    :descriptor-path (:cl-bodge/assets shaders "skeleton")
    :name "skeleton"
    :header "skeleton.h"
    :source "skeleton.glsl"
    :uniforms ("bones"))
