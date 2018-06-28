(cl:in-package :cl-bodge.shading)


(defshader (skinning-shader
            (:name "bodge/skinning")
            (:headers "skinning.h")
            (:sources "skinning.glsl")
            (:base-path :system-relative :cl-bodge/shading "skinning/")))
