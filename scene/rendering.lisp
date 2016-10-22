(in-package :cl-bodge.scene)


(declaim (special
          *shading-pipeline*
          *shading-parameters*))

;;;
;;;
;;;
(defclass shading-parameters ()
  ((bindings :initform (make-hash-table :test #'equal))))


(defun bind-parameter (name shading-program)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (if (null programs)
          (setf programs (list shading-program))
          (push shading-program programs)))))


(defun unbind-parameter (name)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (if (null programs)
          (error "No bindings found for parameter '~a'" name))
          (let ((prog-list programs))
            (if (null (rest prog-list))
                (remhash name bindings)
                (setf programs (rest prog-list)))))))


(defmacro with-bound-parameters ((parameters program) &body body)
  (once-only (parameters program)
    `(progn
       (dolist (name ,parameters)
         (bind-parameter name ,program))
       (unwind-protect
            (progn
              ,@body)
         (dolist (name ,parameters)
           (unbind-parameter name))))))


(defun (setf shading-parameter) (value name)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (when (null programs)
        (error "Parameter with name '~a' is unbound" name))
      (setf (program-uniform-variable (first programs) name) value))))

#++
(defun shading-parameter (name)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (when (null programs)
        (error "Parameter with name '~a' is unbound" name))
      (program-uniform-variable (first programs) name))))


;;;
;;;
;;;
(defclass shading-pipeline-node (node)
  ((pipeline :initarg :pipeline)))


(defmethod rendering-pass ((this shading-pipeline-node))
  (with-slots (pipeline) this
    (let ((old-pipeline (if-unbound *shading-pipeline* nil))
          (*shading-pipeline* pipeline)
          (*shading-parameters* (make-instance 'shading-parameters)))
      (with-bound-shading-pipeline (*shading-pipeline* old-pipeline)
        (call-next-method)))))


;;;
;;;
;;;
(defclass texture-node (node)
  ((tex :initarg :texture)
   (unit :initarg :unit :initform 0)))


(defmethod rendering-pass ((this texture-node))
  (with-slots (tex unit) this
    (with-bound-texture (tex unit)
      (call-next-method))))


;;;
;;;
;;;
(defclass mesh-node (node)
  ((mesh :initarg :mesh)))


(defmethod rendering-pass ((this mesh-node))
  (with-slots (mesh) this
    (render mesh)
    (call-next-method)))


;;;
;;;
;;;
(defclass shading-program-node (node)
  ((program :initarg :program)
   (stages :initarg :stages :initform :all-shader)
   (parameters :initarg :parameters)))


(defmethod rendering-pass ((this shading-program-node))
  (with-slots (program parameters stages) this
    (apply #'use-shading-program-stages (append (list *shading-pipeline* program)
                                                (if (listp stages)
                                                    stages
                                                    (list stages))))
    (with-bound-parameters (parameters program)
      (call-next-method))))

;;;
;;;
;;;
(defclass shading-parameters-node (node)
  ((params :initarg :parameters)))


(defmethod rendering-pass ((this shading-parameters-node))
  (with-slots (params) this
    (loop for (name . value) in params do
         (setf (shading-parameter name) (if (functionp value) (funcall value) value)))
    (call-next-method)))
