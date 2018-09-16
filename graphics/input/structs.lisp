(cl:in-package :cl-bodge.graphics)


(defvar *struct-descriptor-registry* (make-hash-table))


(defun register-shader-struct-descriptor (class descriptor)
  (setf (gethash class *struct-descriptor-registry*) descriptor))


(defun find-shader-struct-descriptor (class)
  (gethash class *struct-descriptor-registry*))


(defstruct shader-struct-field
  (name nil :type symbol :read-only t)
  (type "float" :type string :read-only t)
  (count 1 :type fixnum :read-only t)
  (foreign-name "" :type string :read-only t))


(defun type->glsl (type)
  (eswitch (type :test #'equal)
    (:float "float")
    (:int "int")
    (:uint "uint")
    (:bool "bool")

    (:bool2 "bvec2")
    (:bool3 "bvec3")
    (:bool4 "bvec4")
    (:float2 "vec2")
    (:float3 "vec3")
    (:float4 "vec4")
    (:int2 "ivec2")
    (:int3 "ivec3")
    (:int4 "ivec4")
    (:uint2 "uvec2")
    (:uint3 "uvec3")
    (:uint4 "uvec4")
    (:float3x3 "mat3")
    (:float4x4 "mat4")

    (:bvec2 "bvec2")
    (:bvec3 "bvec3")
    (:bvec4 "bvec4")
    (:vec2 "vec2")
    (:vec3 "vec3")
    (:vec4 "vec4")
    (:ivec2 "ivec2")
    (:ivec3 "ivec3")
    (:ivec4 "ivec4")
    (:uvec2 "uvec2")
    (:uvec3 "uvec3")
    (:uvec4 "uvec4")
    (:mat3 "mat3")
    (:mat4 "mat4")

    (:sampler-2d "sampler2D")
    ('(:sampler-2d :shadow) "sampler2DShadow")
    ('(:sampler-2d :float) "sampler2D")
    ('(:sampler-2d :uint) "usampler2D")
    ('(:sampler-2d :int) "isampler2D")
    (:sampler-cube "samplerCube")))


(defun make-shader-struct-descriptor (fields)
  (loop for field in fields
        collect (destructuring-bind (symbol-name type &key name (count 1) &allow-other-keys) field
                  (let ((foreign-name (or name (string-downcase
                                                (bodge-util:translate-name-to-foreign symbol-name)))))
                    (make-shader-struct-field :name symbol-name
                                              :type (type->glsl type)
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
    (format nil "~A ~A;" field-type full-foreign-name)))


(defun serialize-struct-as-interface (struct-type interface-type block-name &optional (output t))
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
