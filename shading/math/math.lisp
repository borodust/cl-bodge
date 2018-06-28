(cl:in-package :cl-bodge.shading)


(defshader (math-shader
            (:name "bodge/math")
            (:headers "math.h")
            (:base-path :system-relative :cl-bodge/shading "math/")))
