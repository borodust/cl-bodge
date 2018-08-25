(cl:in-package :cl-bodge.shading)


(defshader (depth-shader
            (:sources "depth.glsl")
            (:base-path :system-relative :cl-bodge/shading "shadow/"))
  (depth-position :location 0)
  (depth-mvp :name "MVP"))


(defpipeline depth-pipeline
  :vertex depth-shader
  :fragment depth-shader)


(defun render-with-depth-pipeline (output pipeline mvp-matrix position-buffer
                                   &rest args &key &allow-other-keys)
  (apply #'render output pipeline
         'depth-position position-buffer
         'depth-mvp mvp-matrix
         args))


(defshader (shadow-library
            (:name "bodge/shadow")
            (:headers "shadow.h")
            (:sources "shadow.glsl")
            (:base-path :system-relative :cl-bodge/shading "shadow/")))
