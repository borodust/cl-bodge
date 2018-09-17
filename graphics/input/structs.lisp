(cl:in-package :cl-bodge.graphics)


(defvar *struct-descriptor-registry* (make-hash-table))


(defun register-shader-struct-descriptor (class descriptor)
  (setf (gethash class *struct-descriptor-registry*) descriptor))


(defun find-shader-struct-descriptor (class)
  (gethash class *struct-descriptor-registry*))

;;;
;;; GENERIC GLSL TYPES
;;;
(defclass glsl-type ()
  ((name :initarg :name :reader %glsl-name-of)
   (byte-size :initarg :size :reader %glsl-size-of)
   (byte-alignment :initarg :alignment :reader %glsl-alignment-of)))


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


(defstruct shader-struct-field
  (name nil :type symbol :read-only t)
  (type nil :type (or glsl-type glsl-opaque-type) :read-only t)
  (count 1 :type fixnum :read-only t)
  (foreign-name "" :type string :read-only t))


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


(defun make-shader-struct-descriptor (fields)
  (loop for field in fields
        collect (destructuring-bind (symbol-name type &key name (count 1) &allow-other-keys) field
                  (let ((foreign-name (or name (string-downcase
                                                (bodge-util:translate-name-to-foreign symbol-name)))))
                    (make-shader-struct-field :name symbol-name
                                              :type (make-instance (type->glsl type))
                                              :count count
                                              :foreign-name foreign-name)))))


(defun shader-struct-descriptor-fields (descriptor)
  descriptor)


(defclass shader-structure () ())


(defun fields-to-descriptor (fields)
  (let ((expanded-field-descriptors (loop for field in fields
                                          collect (destructuring-bind (name type &rest opts)
                                                      (ensure-list field)
                                                    `(list ',name ',type ,@opts)))))
    `(make-shader-struct-descriptor (list ,@expanded-field-descriptors))))


(defun fields-to-parameters (fields)
  (loop for field in fields
        collect (first (ensure-list field))))


(defun fields-to-slots (class-name fields)
  (loop for field in fields
        collect (destructuring-bind (symbol-name type &rest opts) (ensure-list field)
                  (declare (ignore type opts))
                  (let ((keyword-name (make-keyword symbol-name)))
                    `(,symbol-name :initform (error ,(with-output-to-string (s)
                                                       (prin1 keyword-name s)
                                                       (format s " missing")))
                                   :initarg ,keyword-name
                                   :accessor ,(symbolicate class-name '- symbol-name))))))


(defmacro defsstruct (name-and-opts &body fields)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (declare (ignore opts))
    (with-gensyms (initargs)
      `(progn
         (defclass ,name (shader-structure)
           (,@(fields-to-slots name fields)))
         (defun ,(symbolicate 'make- name) (&rest ,initargs
                                            &key ,@(fields-to-parameters fields) &allow-other-keys)
           (declare (ignore ,@(fields-to-parameters fields)))
           (apply #'make-instance ',name ,initargs))
         (register-shader-struct-descriptor ',name ,(fields-to-descriptor fields))))))


(defun full-field-name (field)
  (let* ((field-type (shader-struct-field-type field))
         (field-count (shader-struct-field-count field))
         (foreign-name (shader-struct-field-foreign-name field))
         (full-foreign-name (cond
                              ((<= field-count 0)
                               (format nil "~A[]" foreign-name))
                              ((> field-count 1)
                               (format nil "~A[~A]" foreign-name field-count))
                              (t foreign-name))))
    (format nil "~A ~A;" (%glsl-name-of field-type) full-foreign-name)))


(defun serialize-struct-as-interface (struct-type interface-type &optional block-name (output t))
  (let ((descriptor (find-shader-struct-descriptor struct-type)))
    (format output "~&layout(std140) ~A ~A_i {
~{~&  ~A~}
}"
            interface-type
            (translate-name-to-foreign struct-type)
            (loop for field in (shader-struct-descriptor-fields descriptor)
                  collect (full-field-name field)))
    (when block-name
        (format output " ~A" block-name))
      (format output ";")))


(defun serialize-struct-as-uniforms (struct-type &optional (output t))
  (let ((descriptor (find-shader-struct-descriptor struct-type)))
    (loop for field in (shader-struct-descriptor-fields descriptor)
          do (format output "~&uniform ~A" (full-field-name field)))))



(defmethod inject-shader-input ((this shader-structure) &key name)
  (let ((fields (shader-struct-descriptor-fields
                 (find-shader-struct-descriptor (class-name-of this)))))
    (loop for field in fields
          for field-name = (shader-struct-field-name field)
          for foreign-name = (shader-struct-field-foreign-name field)
          for value = (slot-value this field-name)
          do (inject-shader-input value :name (format nil "~A.~A" name foreign-name)))))
