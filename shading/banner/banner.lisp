(cl:in-package :cl-bodge.shading)


(defshader (banner-vertex
            (:sources "banner.v.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/")))


(defshader (banner-fragment
            (:sources "banner.f.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/")))


(defpipeline banner-library
  :vertex-shader banner-vertex
  :fragment-shader banner-shader)
