(in-package :cl-bodge.graphics)

(defclass shading-program (gl-object) ()
  (:default-initargs :id (gl:create-program)))


(define-destructor shading-program ((id id-of) (sys system-of))
  (-> sys
    (gl:delete-program id)))


(defun compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    shader))


(defun make-program (this shader-sources)
  (let ((program (id-of this))
        (shaders (loop for src in shader-sources collect
                      (compile-shader (shader-type-of src) (shader-text-of src)))))
    (loop for shader in shaders do (gl:attach-shader program shader))
    (gl:link-program program)
    (unless (gl:get-program program :link-status)
      (let ((shader-logs (apply #'concatenate 'string
                                (loop for shader in shaders collecting
                                     (format nil "~%~a:~%~a"
                                             (cffi:foreign-enum-keyword
                                              '%gl:enum
                                              (gl:get-shader shader :shader-type))
                                             (gl:get-shader-info-log shader)))))
            (program-log (gl:get-program-info-log program)))
        (error "Program linking failed. Logs:~%~a~%~a" shader-logs program-log)))
    (loop for shader in shaders do
         (gl:detach-shader program shader)
         (gl:delete-shader shader))))


(defmethod initialize-instance :after ((this shading-program) &key shader-sources separable-p)
  (gl:program-parameteri (id-of this) :program-separable separable-p)
  (make-program this shader-sources))


(declaim (inline make-shading-program))
(defun make-shading-program (system &rest shader-sources)
  (make-instance 'shading-program :system system :shader-sources shader-sources))


(declaim (inline make-separable-shading-program))
(defun make-separable-shading-program (system &rest shader-sources)
  (make-instance 'shading-program :system system :shader-sources shader-sources
                 :separable-p t))


(defun use-shading-program (program)
  (gl:use-program (id-of program)))


(defun valid-shading-program-p (program)
  (and (not (null program)) (gl:is-program (id-of program))))


#|
;; fixme: find out appropriate return type
(defun program-uniform-variable (program variable-name)
  (when-let ((variable-idx (gl:get-uniform-location (id-of program) variable-name)))
    (gl:get-active-uniform (id-of program) variable-idx)))
|#

(defun (setf program-uniform-variable) (value program variable-name)
  (when-let ((variable-idx (gl:get-uniform-location (id-of program) variable-name)))
    (etypecase value
      (single-float (gl:program-uniformf (id-of program) variable-idx value))
      (vec (gl:program-uniformfv (id-of program) variable-idx (vec->array value)))
      (square-mat (gl:program-uniform-matrix (id-of program) variable-idx
                                                (square-matrix-size value)
                                                (vector (mat->array value)) nil)))))


;;;
;;; Shading program pipeline
;;;
(defclass shading-pipeline (gl-object) ()
  (:default-initargs :id (gl:gen-program-pipeline)))


(define-destructor shading-pipeline ((id id-of) (sys system-of))
  (-> sys
    (gl:delete-program-pipelines (list id))))


(defun make-shading-pipeline (system)
  (make-instance 'shading-pipeline :system system))

(defmacro with-bound-shading-pipeline ((pipeline &optional previous) &body body)
  (once-only (previous)
    `(unwind-protect
          (progn
            (gl:bind-program-pipeline (id-of ,pipeline))
            ,@body)
       (if (null ,previous)
           (gl:bind-program-pipeline 0)
           (gl:bind-program-pipeline (id-of ,previous))))))


(defun use-shading-program-stages (pipeline program &rest stages)
  (apply #'gl:use-program-stages (id-of pipeline) (id-of program) stages))
