(cl:in-package :cl-bodge.graphics)


(declaim (special *active-shading-program*
                  *next-texture-unit*
                  *buffer-target*))


(definline acceptable-simple-array-p (data)
  (and (typep data 'simple-array) (not (typep data 'simple-vector))))


(defun cl-type->gl (type)
  (switch (type :test (lambda (type super-type) (subtypep type super-type)))
    ('single-float :float)
    ('double-float :double)
    ('(signed-byte 8) :byte)
    ('(unsigned-byte 8) :unsigned-byte)
    ('(signed-byte 16) :short)
    ('(unsigned-byte 16) :unsigned-short)
    ('(signed-byte 32) :int)
    ('(unsigned-byte 32) :unsigned-int)
    ('vec2 :vec2)
    ('vec3 :vec3)
    ('vec4 :vec4)
    ('mat2 :mat2)
    ('mat3 :mat3)
    ('mat4 :mat4)))


(defun gl-type-size (type)
  (ecase type
    (:float 4)
    (:double 8)
    (:byte 1)
    (:unsigned-byte 1)
    (:short 2)
    (:unsigned-short 2)
    (:int 4)
    (:unsigned-int 4)
    (:vec2 (* 4 2))
    (:vec3 (* 4 3))
    (:vec4 (* 4 4))
    (:mat2 (* 4 2 2))
    (:mat3 (* 4 3 3))
    (:mat4 (* 4 4 4))))


(defun gl-type->cl (type)
  (case type
    (:float 'single-float)
    (:double 'double-float)
    (:byte '(signed-byte 8))
    (:unsigned-byte '(unsigned-byte 8))
    (:short '(signed-byte 16))
    (:unsigned-short '(unsigned-byte 16))
    (:int '(signed-byte 32))
    (:unsigned-int '(unsigned-byte 32))
    (:vec2 'vec2)
    (:vec3 'vec3)
    (:vec4 'vec4)
    (:mat2 'mat2)
    (:mat3 'mat3)
    (:mat4 'mat4)
    (t t)))


(defclass shader-input () ())


(defgeneric inject-shader-input (shader-input &rest args &key &allow-other-keys))


(defun next-texture-unit ()
  (prog1 *next-texture-unit*
    (incf *next-texture-unit*)))


(defmacro with-texture-units (() &body body)
  `(let ((*next-texture-unit* 0))
     ,@body))
