(cl:in-package :cl-bodge.graphics)


;;;
;;; GENERIC GLSL TYPES
;;;
(defclass glsl-type ()
  ((name :initarg :name :reader %glsl-name-of)
   (byte-size :initarg :size :reader %glsl-size-of)
   (byte-alignment :initarg :alignment :reader %glsl-alignment-of)))


(defmethod print-object ((this glsl-type) stream)
  (with-slots (byte-size byte-alignment) this
    (format stream "#<~A size: ~A alignment: ~A>" (class-name-of this) byte-size byte-alignment)))


(defclass glsl-single-number (glsl-type) ()
  (:default-initargs :size 4 :alignment 4))


(defclass glsl-gvec2 (glsl-type) ()
  (:default-initargs :size 8 :alignment 8))


(defclass glsl-gvec3 (glsl-type) ()
  (:default-initargs :size 12 :alignment 16))


(defclass glsl-gvec4 (glsl-type) ()
  (:default-initargs :size 16 :alignment 16))

;;;
;;; SCALAR GLSL TYPES
;;;
(defclass glsl-float (glsl-single-number) ()
  (:default-initargs :name "float"))


(defclass glsl-int (glsl-single-number) ()
  (:default-initargs :name "int"))


(defclass glsl-uint (glsl-single-number) ()
  (:default-initargs :name "uint"))


(defclass glsl-bool (glsl-single-number) ()
  (:default-initargs :name "bool"))

;;;
;;; VECTOR GLSL TYPES
;;;
(defclass glsl-vec2 (glsl-gvec2) ()
  (:default-initargs :name "vec2"))


(defclass glsl-vec3 (glsl-gvec3) ()
  (:default-initargs :name "vec3"))


(defclass glsl-vec4 (glsl-gvec4) ()
  (:default-initargs :name "vec4"))


(defclass glsl-ivec2 (glsl-gvec2) ()
  (:default-initargs :name "ivec2"))


(defclass glsl-ivec3 (glsl-gvec3) ()
  (:default-initargs :name "ivec3"))


(defclass glsl-ivec4 (glsl-gvec4) ()
  (:default-initargs :name "ivec4"))


(defclass glsl-uvec2 (glsl-gvec2) ()
  (:default-initargs :name "uvec2"))


(defclass glsl-uvec3 (glsl-gvec3) ()
  (:default-initargs :name "uvec3"))


(defclass glsl-uvec4 (glsl-gvec4) ()
  (:default-initargs :name "uvec4"))


(defclass glsl-bvec2 (glsl-gvec2) ()
  (:default-initargs :name "bvec2"))


(defclass glsl-bvec3 (glsl-gvec3) ()
  (:default-initargs :name "bvec3"))


(defclass glsl-bvec4 (glsl-gvec4) ()
  (:default-initargs :name "bvec4"))


(defclass glsl-mat2 (glsl-type) ()
  (:default-initargs :name "mat2"
                     :size 32
                     :alignment 16))


(defclass glsl-mat3 (glsl-type) ()
  (:default-initargs :name "mat3"
                     :size 48
                     :alignment 16))


(defclass glsl-mat4 (glsl-type) ()
  (:default-initargs :name "mat4"
                     :size 64
                     :alignment 16))


;;;
;;; OPAQUE GLSL TYPES
;;;
(defclass glsl-opaque-type ()
  ((name :initarg :name :reader %glsl-name-of)))


(defclass glsl-sampler-2d (glsl-opaque-type) ()
  (:default-initargs :name "sampler2D"))


(defclass glsl-isampler-2d (glsl-opaque-type) ()
  (:default-initargs :name "isampler2D"))


(defclass glsl-usampler-2d (glsl-opaque-type) ()
  (:default-initargs :name "usampler2D"))


(defclass glsl-sampler-2d-shadow (glsl-opaque-type) ()
  (:default-initargs :name "sampler2DShadow"))


(defclass glsl-sampler-cube (glsl-opaque-type) ()
  (:default-initargs :name "samplerCube"))


(defun type->glsl (type)
  (eswitch (type :test #'equal)
    (:float 'glsl-float)
    (:int 'glsl-int)
    (:uint 'glsl-uint)
    (:bool 'glsl-bool)

    (:bool2 'glsl-bvec2)
    (:bool3 'glsl-bvec3)
    (:bool4 'glsl-bvec4)
    (:float2 'glsl-vec2)
    (:float3 'glsl-vec3)
    (:float4 'glsl-vec4)
    (:int2 'glsl-ivec2)
    (:int3 'glsl-ivec3)
    (:int4 'glsl-ivec4)
    (:uint2 'glsl-uvec2)
    (:uint3 'glsl-uvec3)
    (:uint4 'glsl-uvec4)
    (:float3x3 'glsl-mat3)
    (:float4x4 'glsl-mat4)

    (:bvec2 'glsl-bvec2)
    (:bvec3 'glsl-bvec3)
    (:bvec4 'glsl-bvec4)
    (:vec2 'glsl-vec2)
    (:vec3 'glsl-vec3)
    (:vec4 'glsl-vec4)
    (:ivec2 'glsl-ivec2)
    (:ivec3 'glsl-ivec3)
    (:ivec4 'glsl-ivec4)
    (:uvec2 'glsl-uvec2)
    (:uvec3 'glsl-uvec3)
    (:uvec4 'glsl-uvec4)
    (:mat3 'glsl-mat3)
    (:mat4 'glsl-mat4)

    (:sampler-2d 'glsl-sampler-2d)
    ('(:sampler-2d :shadow) 'glsl-sampler-2d-shadow)
    ('(:sampler-2d :float) 'glsl-sampler-2d)
    ('(:sampler-2d :uint) 'glsl-usampler-2d)
    ('(:sampler-2d :int) 'glsl-isampler-2d)
    (:sampler-cube 'glsl-sampler-cube)))


(defmethod inject-shader-input (shader-input &key type name)
  (case type
    (:bool (let ((location (gl:get-uniform-location *active-shading-program* name)))
             (gl:uniformi location (if shader-input 1 0))))
    (t (error "Don't know how to inject shader input of type ~A (provided GLSL type: ~A)"
              (type-of shader-input) type))))
