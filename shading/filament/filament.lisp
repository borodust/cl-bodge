(cl:in-package :cl-bodge.shading)

;;;
;;; Shader code adapted from Google's Filament engine: https://github.com/google/filament
;;; Check out src/LICENSE file for licensing details
;;;

(define-constant +max-light-count+ 8)

(defshader (filament-vertex
            (:name "bodge/filament")
            (:sources "filament.v.glsl")
            (:base-path :system-relative :cl-bodge/shading "filament/")))


(defshader (filament-fragment
            (:name "bodge/filament")
            (:sources "filament.f.glsl")
            (:base-path :system-relative :cl-bodge/shading "filament/")))


(defsstruct filament-frame-uniforms
  (view-from-world-matrix :mat4 :name "viewFromWorldMatrix")
  (world-from-view-matrix :mat4 :name "worldFromViewMatrix")
  (clip-from-view-matrix :mat4 :name "clipFromViewMatrix")
  (view-from-clip-matrix :mat4 :name "viewFromClipMatrix")
  (clip-from-world-matrix :mat4 :name "clipFromWorldMatrix")
  (light-from-world-matrix :mat4 :name "lightFromWorldMatrix")
  (resolution :vec4 :name "resolution")
  (camera-position :vec3 :name "cameraPosition")
  (time :float :name "time")
  (light-color-intensity :vec4 :name "lightColorIntensity")
  (sun :vec4  :name "sun")
  (light-direction :vec3 :name "lightDirection")
  (f-params-x :uint :name "fParamsX")
  (shadow-bias :vec3 :name "shadowBias")
  (one-over-froxel-dimension-y :float :name "oneOverFroxelDimensionY")
  (z-params :vec4 :name "zParams")
  (f-params :ivec2 :name "fParams")
  (origin :vec2 :name "origin")
  (one-over-froxel-dimension :float :name "oneOverFroxelDimension")
  (ibl-luminance :float :name "iblLuminance")
  (exposure :float :name "exposure")
  (ev100 :float :name "ev100")
  (ibl-sh :vec3 :name "iblSH" :count 9))


(defsstruct filament-object-uniforms
  (world-from-model-matrix :mat4 :name "worldFromModelMatrix")
  (world-from-model-normal-matrix :mat3 :name "worldFromModelNormalMatrix"))


(defsstruct filament-lights-uniforms
  (lights :mat4 :name "lights" :count +max-light-count+))


(defsstruct filament-material-params)


(defsstruct filament-light
  (shadowMap (:sampler-2d :shadow) :name "light_shadowMap")
  (records (:sampler-2d :uint) :name "light_records")
  (froxels (:sampler-2d :uint) :name "light_froxels")
  (iblDFG :sampler-2d :name "light_iblDFG")
  (iblSpecular :sampler-cube :name "light_iblSpecular"))


(defpipeline filament-pipeline
  :vertex filament-vertex
  :fragment filament-fragment)
