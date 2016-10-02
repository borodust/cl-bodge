(in-package :cl-bodge.graphics)

(defclass shading-program (gl-object)
  ()
  (:default-initargs :id (gl:create-program)))


(Defun read-file-into-string-list (pathname)
  (split-sequence:split-sequence "#\Newline" (read-file-into-string pathname)))


(defun compile-shader (type source-pathname)
  (let* ((shader-src (read-file-into-string-list source-pathname))
	 (shader (gl:create-shader type)))
    (gl:shader-source shader shader-src)
    (gl:compile-shader shader)
    (log:debug "Compilation log for ~a:~%~a" source-pathname (gl:get-shader-info-log shader))
    shader))


(defun make-program (program shader-path-alist)
  (let* ((shaders (loop for (shader-type . source-path) in shader-path-alist collect
		       (compile-shader shader-type source-path))))
    (loop for shader in shaders do (gl:attach-shader program shader))
    (gl:link-program program)
    (log:debug "Program log:~%~a" (gl:get-program-info-log program))
    (loop for shader in shaders do (gl:delete-shader shader))))


(defmethod initialize-instance :after ((this shading-program) &key shader-path-alist)
  (make-program (id-of this) shader-path-alist))


(defun make-shading-program (shader-path-alist)
  (make-instance 'shading-program :shader-path-alist shader-path-alist))


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
      (vec (gl:uniformfv variable-idx (vector->array value)))
      (mat4 (gl:uniform-matrix variable-idx 4 (vector (matrix->array value)) nil)))))
