(cl:in-package :cl-bodge.graphics)


(defgeneric shader-descriptor-last-updated (descriptor))


(defstruct shader-library
  (descriptor nil :read-only t)
  (dirty-p t)
  (last-compiled 0d0 :type double-float)
  (dependencies nil)
  (cache nil))


(defclass shader-registry ()
  ((library-table :initform (make-hash-table))
   (name-table :initform (make-hash-table :test #'equalp))
   (dependency-table :initform (make-hash-table))))


(defvar *shader-registry* (make-instance 'shader-registry))


(defun register-shader-library (shader-class)
  (with-slots (library-table name-table) *shader-registry*
    (with-hash-entries ((shader-library shader-class)) library-table
      (if shader-library
          (let ((library-name (%name-of (shader-library-descriptor shader-library))))
            (setf (gethash library-name name-table) shader-class))
          (let ((descriptor (make-instance shader-class)))
            (setf shader-library (make-shader-library :descriptor descriptor)
                  (gethash (%name-of descriptor) name-table) shader-class))))))


(defun shader-library-last-updated (shader-class)
  (shader-descriptor-last-updated (shader-library-descriptor
                                   (find-shader-library shader-class))))


(defun find-shader-library (shader-class)
  (with-slots (library-table) *shader-registry*
    (gethash shader-class library-table)))


(defun find-shader-library-by-name (name)
  (with-slots (name-table) *shader-registry*
    (find-shader-library (gethash name name-table))))


(defun find-compiled-shader (shader-class type)
  (when-let ((library (find-shader-library shader-class)))
    (assoc-value (shader-library-cache library) type)))


(defun shader-library-parameters (shader-class)
  (when-let ((library (find-shader-library shader-class)))
    (shader-descriptor-parameters (shader-library-descriptor library))))


(defun clear-shader-library-cache (shader-library)
  (loop for (nil . shader-id) in (shader-library-cache shader-library)
        do (gl:delete-shader shader-id))
  (setf (shader-library-cache shader-library) nil))


(defun remove-from-dependencies (shader-class dependencies)
  (with-slots (dependency-table) *shader-registry*
    (loop for dependency in dependencies do
      (deletef (gethash dependency dependency-table) shader-class))))


(defun register-dependencies (shader-class dependencies)
  (with-slots (dependency-table) *shader-registry*
    (loop for dependency in dependencies
          unless (eq dependency shader-class)
            do (pushnew shader-class (gethash dependency dependency-table)))))


(defun mark-dependent-dirty (shader-class)
  (with-slots (dependency-table) *shader-registry*
    (loop for dependent in (gethash shader-class dependency-table)
          for library = (find-shader-library dependent)
          when library
            do (setf (shader-library-dirty-p library) t)
               (mark-dependent-dirty dependent))))


(defenum shader-type
  :vertex-shader
  :tessellation-control-shader
  :tessellation-evaluation-shader
  :geometry-shader
  :fragment-shader
  :compute-shader)


(defun shader-type->gl (type)
  (ecase type
    ((:vertex-shader :geometry-shader :fragment-shader) type)
    (:tessellation-control-shader :tess-control-shader)
    (:tessellation-evaluation-shader :tess-evaluation-shader)))


(defun compile-shader (name source type)
  (let ((shader (gl:create-shader (shader-type->gl type))))
    (handler-bind ((serious-condition (lambda (c) (declare (ignore c)) (gl:delete-shader shader))))
      (gl:shader-source shader source)
      (gl:compile-shader shader)
      (unless (gl:get-shader shader :compile-status)
        (error "~A '~A' (id = ~A) compilation failed:~%~A"
               type name shader (gl:get-shader-info-log shader)))
      shader)))


(defun refresh-compiled-shader (library type)
  (let* ((descriptor (shader-library-descriptor library))
         (shader-class (class-name-of descriptor)))
    (multiple-value-bind (source dependencies)
        (preprocess-shader descriptor type)
      (register-dependencies shader-class dependencies)
      (let ((shader-id (compile-shader (%name-of descriptor) source type)))
        (setf (assoc-value (shader-library-cache library) type) shader-id
              (shader-library-dirty-p library) nil
              (shader-library-last-compiled library) (float (real-time-seconds) 0d0))
        shader-id))))


(defun collect-compiled-libraries (shader-class type)
  (when-let ((library (find-shader-library shader-class)))
    (let ((descriptor (shader-library-descriptor library)))
      (when (> (shader-descriptor-last-updated descriptor)
               (shader-library-last-compiled library))
        (remove-from-dependencies shader-class (shader-library-dependencies library))
        (mark-dependent-dirty shader-class)
        (clear-shader-library-cache library))
      (let ((shader-id (if-let ((compiled-shader (find-compiled-shader shader-class type)))
                         compiled-shader
                         (refresh-compiled-shader library type))))
        (nconc (list shader-id) (loop for dependency in (shader-library-dependencies library)
                                      nconc (collect-compiled-libraries dependency type)))))))


(defun link-shader-libraries (&rest libraries &key &allow-other-keys)
  (let ((shaders (loop for (type shader-class) on libraries by #'cddr
                       append (collect-compiled-libraries shader-class type)))
        (program (gl:create-program)))
    (handler-bind ((serious-condition (lambda (c)
                                        (declare (ignore c)) (gl:delete-program program))))
      (unwind-protect
           (progn
             (loop for shader-id in shaders do
               (gl:attach-shader program shader-id))
             (gl:link-program program)
             (unless (gl:get-program program :link-status)
               (error "Program linking failed:~%~a" (gl:get-program-info-log program))))
        (loop for shader-id in shaders do
          (gl:detach-shader program shader-id))))
    program))
