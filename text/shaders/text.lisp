(cl:in-package :cl-bodge.text)


(defshader (text-library
            (:name "bodge/text")
            (:headers "text.h")
            (:sources "text.glsl")
            (:base-path :system-relative :cl-bodge/text "shaders/")))


(defshader (text-vertex
            (:sources "text.v.glsl")
            (:base-path :system-relative :cl-bodge/text "shaders/"))
  (box :location 0 :size 4)
  (sdf-coord :location 1 :size 4))


(defshader (text-geometry
            (:sources "text.g.glsl")
            (:base-path :system-relative :cl-bodge/text "shaders/"))
  (mvp :name "proj")
  (scale :name "scale"))


(defshader (text-fragment
            (:sources "text.f.glsl")
            (:base-path :system-relative :cl-bodge/text "shaders/"))
  (atlas :name "atlas")
  (base-color :name "baseColor"))


(defpipeline (text-pipeline
              (:primitive :points))
  :vertex text-vertex
  :geometry text-geometry
  :fragment text-fragment)
