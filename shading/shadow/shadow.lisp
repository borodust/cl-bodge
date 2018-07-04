(cl:in-package :cl-bodge.shading)


(defshader (depth-vertex
            (:sources "depth.v.glsl")
            (:base-path :system-relative :cl-bodge/shading "shadow/"))
  (depth-mvp :name "MVP"))


(defshader (depth-fragment
            (:sources "depth.f.glsl")
            (:base-path :system-relative :cl-bodge/shading "shadow/")))


(defpipeline depth-pipeline
  :vertex depth-vertex
  :fragment depth-fragment)
