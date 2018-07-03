(cl:in-package :cl-bodge.shading)


(defshader (banner-vertex
            (:sources "banner.v.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/"))
  (banner-position :location 0)
  (banner-tex-coord :location 1))


(defshader (banner-geometry
            (:sources "banner.g.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/"))
  (banner-mvp :name "MVP"))


(defshader (banner-fragment
            (:sources "banner.f.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/"))
  (banner-texture :location 1 :name "banner"))


(defpipeline (banner-pipeline
              (:primitive :points))
  :vertex banner-vertex
  :geometry banner-geometry
  :fragment banner-fragment)
