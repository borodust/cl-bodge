(in-package :cl-bodge.graphics)


(declaim (special *active-shading-program*
                  *active-shading-pipeline*))


(defun shader-type->gl (type)
  (ecase type
    ((:vertex-shader :geometry-shader :fragment-shader) type)
    (:tessellation-control-shader :tess-control-shader)
    (:tessellation-evaluation-shader :tess-evaluation-shader)))


(defhandle shader-handle
    :closeform (gl:delete-shader *handle-value*))


(defclass shader (gl-object)
  ((type :initarg :type :reader shader-type-of)))


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
  (call-next-method this
                    :handle (make-shader-handle (%compile-shader type source name))
                    :system system :type type))


(define-system-function compile-shader graphics-system (shader-source &key (system *system*))
  (restart-case
      (make-instance 'shader
                     :type (shader-type-of shader-source)
                     :name (shader-name-of shader-source)
                     :source (shader-text-of shader-source)
                     :system system)
    (reload-source-and-compile ()
      (compile-shader (reload-shader-text shader-source)))))


;;;
;;;
;;;
(defhandle shading-progrm-handle
    :initform (gl:create-program)
    :closeform (gl:delete-program *handle-value*))


(defstruct (uniform-descriptor
             (:conc-name uniform-)
             (:constructor make-uniform-descriptor (name type size)))
  (name nil :read-only t)
  (type nil :read-only t)
  (size nil :read-only t))


(defclass shading-program (gl-object)
  ((uniforms :initform nil :reader uniforms-of))
  (:default-initargs :handle (make-shading-progrm-handle)))


(defun introspect-program (shading-program)
  (with-slots (uniforms) shading-program
    (let* ((pid (handle-value-of shading-program))
           (uniform-count (gl:get-program pid :active-uniforms)))
      (setf uniforms (loop for uniform-id from 0 below uniform-count collect
                          (multiple-value-bind (usize utype uname)
                              (gl:get-active-uniform pid uniform-id)
                            (make-uniform-descriptor uname utype usize)))))))


(defun %make-program (this shader-sources precompiled-shaders)
  (let* ((program (handle-value-of this))
         (shaders (loop for src in shader-sources collect
                       (compile-shader src)))
         (all-shaders (append precompiled-shaders shaders)))
    (unwind-protect
         (loop for shader in all-shaders do
              (gl:attach-shader program (handle-value-of shader)))
      (gl:link-program program)
      (unless (gl:get-program program :link-status)
        (error "Program linking failed:~%~a" (gl:get-program-info-log program)))
      (loop for shader in all-shaders do
           (gl:detach-shader program (handle-value-of shader)))
      (loop for shader in shaders do
           (dispose shader)))))


(defmethod initialize-instance :after ((this shading-program)
                                       &key shader-sources shaders separable-p)
  (gl:program-parameteri (handle-value-of this) :program-separable separable-p)
  (%make-program this shader-sources shaders)
  (introspect-program this))


(define-system-function make-shading-program graphics-system
    (shader-sources &key (system *system*))
  (make-instance 'shading-program :system system :shader-sources shader-sources))


(define-system-function make-separable-shading-program graphics-system
    (shader-sources &key (system *system*))
  (make-instance 'shading-program :system system :shader-sources shader-sources
                 :separable-p t))


(define-system-function link-separable-shading-program graphics-system
    (shaders &key (system *system*))
  (make-instance 'shading-program :system system :shaders shaders
                 :separable-p t))


(define-system-function build-separable-shading-program graphics-system
    (shader-sources shaders &key (system *system*))
  (make-instance 'shading-program :system system :shaders shaders
                 :shader-sources shader-sources :separable-p t))


(defun use-shading-program (program)
  (gl:use-program (handle-value-of program)))


(defmacro with-active-shading-program ((program &optional prev-program) &body body)
  (once-only (program)
    `(unwind-protect
          (let ((*active-shading-program* ,program))
            (use-shading-program ,program)
            ,@body)
       ,(if (null prev-program)
            `(if-bound *active-shading-program*
                       (gl:use-program (handle-value-of *active-shading-program*))
                       (gl:use-program 0))
            `(gl:use-program (handle-value-of ,prev-program))))))


(defun valid-shading-program-p (program)
  (and (not (null program)) (gl:is-program (handle-value-of program))))


#|
;; fixme: find out appropriate return type
(defun program-uniform-variable (program variable-name)
  (when-let ((variable-idx (gl:get-uniform-location (handle-value-of program) variable-name)))
    (gl:get-uniform (handle-value-of program) variable-idx)))
|#

(defun (setf program-uniform-variable) (value program variable-name)
  (when-let ((variable-idx (gl:get-uniform-location (handle-value-of program) variable-name)))
    (etypecase value
      (integer (gl:program-uniformi (handle-value-of program) variable-idx value))
      (single-float (gl:program-uniformf (handle-value-of program) variable-idx value))
      (vec (gl:program-uniformfv (handle-value-of program) variable-idx (vec->array value)))
      (square-mat (gl:program-uniform-matrix (handle-value-of program) variable-idx
                                             (square-matrix-size value)
                                             (vector (mat->array value)) nil)))))


;;;
;;; Shading program pipeline
;;;
(defhandle shading-pipeline-handle
    :initform (gl:gen-program-pipeline)
    :closeform (gl:delete-program-pipelines (list *handle-value*)))


(defclass shading-pipeline (gl-object) ()
  (:default-initargs :handle (make-shading-pipeline-handle)))


(define-system-function make-shading-pipeline graphics-system (&key (system *system*))
  (make-instance 'shading-pipeline :system system))


(defmacro with-bound-shading-pipeline ((pipeline) &body body)
  (once-only (pipeline)
    `(unwind-protect
          (let ((*active-shading-pipeline* ,pipeline))
            (gl:bind-program-pipeline (handle-value-of ,pipeline))
            ,@body)
       (if-bound *active-shading-pipeline*
           (gl:bind-program-pipeline (handle-value-of *active-shading-pipeline*))
           (gl:bind-program-pipeline 0)))))


(defun use-shading-program-stages (pipeline program &rest stages)
  (apply #'gl:use-program-stages (handle-value-of pipeline) (handle-value-of program) stages))
