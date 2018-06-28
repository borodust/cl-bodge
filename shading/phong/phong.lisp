(cl:in-package :cl-bodge.shading)


(defshader (phong-shader
            (:name "bodge/phong")
            (:headers "phong.h")
            (:sources "phong.glsl")
            (:base-path :system-relative :cl-bodge/shading "phong/")))
