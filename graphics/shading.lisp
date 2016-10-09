(in-package :cl-bodge.graphics)

(defclass shading-program (gl-object)
  ()
  (:default-initargs :id (gl:create-program)))



(defun compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (log:trace "Compilation log for ~a:~%~a" shader (gl:get-shader-info-log shader))
    shader))


(defun make-program (program shader-sources)
  (let* ((shaders (loop for src in shader-sources collect
		       (compile-shader (shader-type-of src) (shader-text-of src)))))
    (loop for shader in shaders do (gl:attach-shader program shader))
    (gl:link-program program)
    (log:trace "Program log:~%~a" (gl:get-program-info-log program))
    (loop for shader in shaders do (gl:delete-shader shader))))


(defmethod initialize-instance :after ((this shading-program) &key shader-sources)
  (make-program (id-of this) shader-sources))


(declaim (inline make-shading-program))
(defun make-shading-program (&rest shader-sources)
  (make-instance 'shading-program :shader-sources shader-sources))


(defun use-shading-program (program)
  (gl:use-program (id-of program)))


(defun valid-shading-program-p (program)
  (and (not (null program)) (gl:is-program (id-of program))))


(defun program-uniform-variable (program variable-name)
  (when-let ((variable-idx (gl:get-uniform-location (id-of program) variable-name)))
    (gl:get-active-uniform (id-of program) variable-idx)))


(defun (setf program-uniform-variable) (value program variable-name)
  (when-let ((variable-idx (gl:get-uniform-location (id-of program) variable-name)))
    (etypecase value
      (single-float (gl:uniformf variable-idx value))
      (vec (gl:uniformfv variable-idx (vec->array value)))
      (mat4 (gl:uniform-matrix variable-idx 4 (vector (mat->array value)) nil)))))
