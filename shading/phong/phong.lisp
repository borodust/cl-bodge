(cl:in-package :cl-bodge.shading)


(defsstruct phong-point-light
  position
  color
  ambient
  falloff
  radius)


(defsstruct phong-material
  (specular-scale :name "specularScale")
  shininess
  roughness
  albedo)


(defshader (phong-shader
            (:name "bodge/phong")
            (:headers "phong.h")
            (:sources "phong.glsl")
            (:base-path :system-relative :cl-bodge/shading "phong/")))
