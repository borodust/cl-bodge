(in-package :cl-bodge.assets)


(define-shader-library math-library
    :descriptor-path (:cl-bodge/assets shaders "math")
    :name "math"
    :header "math.h")
