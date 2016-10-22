(in-package :cl-bodge.graphics)


(defclass shader (gl-object)
  ((type :initarg :type :reader shader-type-of)))


(define-destructor shader ((id id-of) (sys system-of))
  (-> sys
    (gl:delete-shader id)))


(defun %compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    shader))


(defmethod initialize-instance ((this shader) &key type source system)
  (call-next-method this :id (%compile-shader type source) :system system :type type))


(definline compile-shader (system type source)
  (make-instance 'shader :type type :source source :system system))


;;;
;;;
;;;
(defclass shading-program (gl-object) ()
  (:default-initargs :id (gl:create-program)))


(define-destructor shading-program ((id id-of) (sys system-of))
  (-> sys
    (gl:delete-program id)))


(defun %make-program (this shader-sources precompiled-shaders)
  (let* ((program (id-of this))
         (shaders (loop for src in shader-sources collect
                       (compile-shader (system-of this)
                                       (shader-type-of src)
                                       (shader-text-of src))))
         (all-shaders (append precompiled-shaders shaders)))
    (unwind-protect
         (loop for shader in all-shaders do
              (gl:attach-shader program (id-of shader)))
      (gl:link-program program)
      (unless (gl:get-program program :link-status)
        (let ((shader-logs (apply #'concatenate 'string
                                  (loop for shader in all-shaders collecting
                                       (format nil "~%~a:~%~a"
                                               (shader-type-of shader)
                                               (gl:get-shader-info-log (id-of shader))))))
              (program-log (gl:get-program-info-log program)))
          (error "Program linking failed. Logs:~%~a~%~a" shader-logs program-log)))
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
