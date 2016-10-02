(in-package :cl-bodge.math)

;;; Vectors

(deftype vec2 ()
  'sb-cga:vec2)

(deftype vec3 ()
  'sb-cga:vec)

(deftype vec4 ()
  'sb-cga:vec4)

(deftype vec ()
  '(or vec2 vec3 vec4))


;;; Matricies

(deftype mat4 ()
  'sb-cga:matrix)

(deftype matrix ()
  '(or mat4))
