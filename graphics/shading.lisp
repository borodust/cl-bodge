(in-package :cl-bodge.graphics)


(declaim (special *active-shading-program*
                  *active-shading-pipeline*))


(defun shader-type->gl (type)
  (ecase type
    ((:vertex-shader :geometry-shader :fragment-shader) type)
    (:tessellation-control-shader :tess-control-shader)
    (:tessellation-evaluation-shader :tess-evaluation-shader)))


(defclass shader (gl-object)
  ((type :initarg :type :reader shader-type-of)))


(define-destructor shader ((id id-of) (sys system-of))
  (-> (sys)
    (gl:delete-shader id)))


(defun %compile-shader (type source name)
  (let ((shader (gl:create-shader (shader-type->gl type))))
    (handler-bind ((t (lambda (c) (declare (ignore c)) (gl:delete-shader shader))))
      (gl:shader-source shader source)
      (gl:compile-shader shader)
      (unless (gl:get-shader shader :compile-status)
        (error "~a '~a' (id = ~a) compilation failed:~%~a"
               type name shader (gl:get-shader-info-log shader)))
      shader)))


(defmethod initialize-instance ((this shader) &key type source system (name ""))
  (call-next-method this :id (%compile-shader type source name) :system system :type type))


(defun compile-shader (system shader-source)
  (restart-case
      (make-instance 'shader
                     :type (shader-type-of shader-source)
                     :name (shader-name-of shader-source)
                     :source (shader-text-of shader-source)
                     :system system)
    (reload-source-and-compile ()
      (compile-shader system (reload-shader-text shader-source)))))


;;;
;;;
;;;
(defclass shading-program (gl-object) ()
  (:default-initargs :id (gl:create-program)))


(define-destructor shading-program ((id id-of) (sys system-of))
  (-> (sys)
    (gl:delete-program id)))


(defun %make-program (this shader-sources precompiled-shaders)
  (let* ((program (id-of this))
         (shaders (loop for src in shader-sources collect
                       (compile-shader (system-of this) src)))
         (all-shaders (append precompiled-shaders shaders)))
    (unwind-protect
         (loop for shader in all-shaders do
              (gl:attach-shader program (id-of shader)))
      (gl:link-program program)
      (unless (gl:get-program program :link-status)
        (error "Program linking failed:~%~a" (gl:get-program-info-log program)))
      (loop for shader in all-shaders do
           (gl:detach-shader program (id-of shader)))
      (loop for shader in shaders do
           (dispose shader)))))


(defmethod initialize-instance :after ((this shading-program)
                                       &key shader-sources shaders separable-p)
  (gl:program-parameteri (id-of this) :program-separable separable-p)
  (%make-program this shader-sources shaders))


(definline make-shading-program (system &rest shader-sources)
  (make-instance 'shading-program :system system :shader-sources shader-sources))


(definline make-separable-shading-program (system &rest shader-sources)
  (make-instance 'shading-program :system system :shader-sources shader-sources
                 :separable-p t))


(definline link-separable-shading-program (system &rest shaders)
  (make-instance 'shading-program :system system :shaders shaders
                 :separable-p t))


(definline build-separable-shading-program (system shader-sources shaders)
  (make-instance 'shading-program :system system :shaders shaders
                 :shader-sources shader-sources :separable-p t))


(defun use-shading-program (program)
  (gl:use-program (id-of program)))


(defmacro with-using-shading-program ((program &optional prev-program) &body body)
  (once-only (program)
    `(unwind-protect
          (let ((*active-shading-program* ,program))
            (use-shading-program ,program)
            ,@body)
       ,(if (null prev-program)
            `(if-bound *active-shading-program*
                       (gl:use-program (id-of *active-shading-program*))
                       (gl:use-program 0))
            `(gl:use-program (id-of ,prev-program))))))


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
      (integer (gl:program-uniformi (id-of program) variable-idx value))
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
  (-> (sys)
    (gl:delete-program-pipelines (list id))))


(defun make-shading-pipeline (system)
  (make-instance 'shading-pipeline :system system))

(defmacro with-bound-shading-pipeline ((pipeline) &body body)
  (once-only (pipeline)
    `(unwind-protect
          (let ((*active-shading-pipeline* ,pipeline))
            (gl:bind-program-pipeline (id-of ,pipeline))
            ,@body)
       (if-bound *active-shading-pipeline*
           (gl:bind-program-pipeline (id-of *active-shading-pipeline*))
           (gl:bind-program-pipeline 0)))))


(defun use-shading-program-stages (pipeline program &rest stages)
  (apply #'gl:use-program-stages (id-of pipeline) (id-of program) stages))
