(in-package :cl-bodge.math)

(defgeneric lerp (this that f))
(defgeneric multiply (this that))
(defgeneric summarize (this that))
(defgeneric divide (this that))
(defgeneric subtract (this that))


(macrolet ((defreduced (name generic)
             `(defun ,name (arg0 &rest args)
                (reduce (lambda (this that) (,generic this that))
                        args :initial-value arg0))))

  (defreduced mult multiply)
  (defreduced sum summarize)
  (defreduced div divide)
  (defreduced subt subtract))


;;;
;;; Vectors
;;;
(defclass vec () ())


(defclass vec2 (vec)
  ((value :initarg :value :initform (sb-cga:vec2 #f0 #f0) :type sb-cga:vec2
          :reader value-of)))


(defclass vec3 (vec)
  ((value :initarg :value :initform (sb-cga:vec #f0 #f0 #f0) :type sb-cga:vec
          :reader value-of)))


(defclass vec4 (vec)
  ((value :initarg :value :initform (sb-cga:vec4 #f0 #f0 #f0 #f0) :type sb-cga:vec4
          :reader value-of)))

;;;
;;; Matricies
;;;
(defclass mat () ())


(defclass square-mat (mat) ())


(defclass mat2 (square-mat)
  ((value :initarg :value
          :initform (make-array 4 :element-type 'single-float :initial-element #f0)
          :type (simple-array single-float (4)) :reader value-of)))


(defclass mat3 (square-mat)
  ((value :initarg :value
          :initform (make-array 9 :element-type 'single-float :initial-element #f0)
          :type (simple-array single-float (9)) :reader value-of)))


(defclass mat4 (square-mat)
  ((value :initarg :value
          :initform (sb-cga:zero-matrix) :type sb-cga:matrix :reader value-of)))


(declaim (ftype (function (square-mat) (integer 2 4)) square-matrix-size))
(definline square-matrix-size (square-mat)
  (etypecase square-mat
    (mat2 2)
    (mat3 3)
    (mat4 4)))


;;;
;;; Quaternions
;;;
(defclass quaternion ()
  ((value :initarg :value :type (simple-array single-float (4)))))
