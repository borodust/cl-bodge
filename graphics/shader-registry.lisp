(cl:in-package :cl-bodge.graphics)


(defgeneric shader-descriptor-last-updated (descriptor))


(defstruct shader-library
  (descriptor nil :read-only t)
  (dirty-p t)
  (dependencies nil)
  (cache nil))


(defclass shader-registry ()
  ((library-table :initform (make-hash-table))
   (name-table :initform (make-hash-table :test #'equalp))
   (dependency-table :initform (make-hash-table))
   (dirty-lock :initform (mt:make-spin-lock))))


(defvar *shader-registry* (make-instance 'shader-registry))


(defun find-shader-library (shader-class)
  (with-slots (library-table) *shader-registry*
    (gethash shader-class library-table)))


(defun find-shader-library-by-name (name)
  (with-slots (name-table) *shader-registry*
    (find-shader-library (gethash name name-table))))


(defun find-compiled-shader (shader-class type &rest opts &key &allow-other-keys)
  (when-let ((library (find-shader-library shader-class)))
    (assoc-value (shader-library-cache library) (nconc (list type) (sort-opts opts)) :test #'equal)))


(defun shader-library-parameters (shader-class)
  (when-let ((library (find-shader-library shader-class)))
    (shader-descriptor-parameters (shader-library-descriptor library))))


(defun clear-shader-library-cache (shader-library)
  (loop for (nil . shader-id) in (shader-library-cache shader-library)
        do (gl:delete-shader shader-id))
  (setf (shader-library-cache shader-library) nil))


(defun clear-registry-cache ()
  (with-slots (library-table) *shader-registry*
    (loop for shader-library being the hash-values of library-table
          do (clear-shader-library-cache shader-library))))


(defun remove-from-dependencies (shader-class dependencies)
  (with-slots (dependency-table) *shader-registry*
    (loop for dependency in dependencies do
      (deletef (gethash dependency dependency-table) shader-class))))


(defun register-dependencies (shader-class dependencies)
  (with-slots (dependency-table) *shader-registry*
    (loop for dependency in dependencies
          unless (eq dependency shader-class)
            do (pushnew shader-class (gethash dependency dependency-table)))))


(defun mark-dirty (shader-class)
  (with-slots (dependency-table dirty-lock) *shader-registry*
    (mt:with-spin-lock-held (dirty-lock)
      (labels ((%mark-dirty (shader-class)
                 (when-let ((library (find-shader-library shader-class)))
                   (setf (shader-library-dirty-p library) t)
                   (loop for dependent in (gethash shader-class dependency-table)
                         do (%mark-dirty dependent)))))
        (%mark-dirty shader-class)))))


(defun clean-if-dirty (library)
  (with-slots (dirty-lock) *shader-registry*
    (mt:with-spin-lock-held (dirty-lock)
      (when (shader-library-dirty-p library)
        (clear-shader-library-cache library)
        (setf (shader-library-dirty-p library) nil)
        t))))


(defun reload-all-shader-sources ()
  (with-slots (library-table) *shader-registry*
    (loop for library being the hash-values of library-table
          do (reload-shader-sources (shader-library-descriptor library)))))


(defun reload-changed-and-mark-dirty ()
  (with-slots (library-table) *shader-registry*
    (loop with reloaded
          for library being the hash-values of library-table
          for descriptor = (shader-library-descriptor library)
          for descriptor-class-name = (class-name-of descriptor)
          when (shader-changed-on-disk-p descriptor)
            do (reload-shader-sources descriptor)
               (mark-dirty descriptor-class-name)
               (push descriptor-class-name reloaded)
          finally (return reloaded))))


(defun register-shader-library (shader-class)
  (with-slots (library-table name-table) *shader-registry*
    (with-hash-entries ((shader-library shader-class)) library-table
      (if shader-library
          (let* ((descriptor (shader-library-descriptor shader-library))
                 (library-name (%name-of descriptor)))
            (reload-shader-sources descriptor)
            (setf (gethash library-name name-table) shader-class)
            (mark-dirty shader-class))
          (let ((descriptor (make-instance shader-class)))
            (setf shader-library (make-shader-library :descriptor descriptor)
                  (gethash (%name-of descriptor) name-table) shader-class))))))


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
    (handler-bind ((serious-condition (lambda (c)
                                        (declare (ignore c))
                                        (gl:delete-shader shader))))
      (gl:shader-source shader source)
      (gl:compile-shader shader)
      (unless (gl:get-shader shader :compile-status)
        (error "~A '~A' (id = ~A) compilation failed:~&~A~&"
               type name shader (gl:get-shader-info-log shader))))
    shader))


(defun sort-opts (opts)
  (alist-plist (sort (plist-alist opts) #'string< :key #'car)))


(defun refresh-compiled-shader (library type &rest opts &key &allow-other-keys)
  (let (shader-id
        (opts (sort-opts opts)))
    (tagbody start
       (restart-case
           (let* ((descriptor (shader-library-descriptor library)))
             (multiple-value-bind (source dependencies)
                 (apply #'preprocess-shader descriptor type opts)
               (register-dependencies (class-name-of (shader-library-descriptor library)) dependencies)
               (setf (shader-library-dependencies library) dependencies
                     shader-id (compile-shader (%name-of descriptor) source type)
                     (assoc-value (shader-library-cache library) (nconc (list type) opts)) shader-id)))
         (recompile ()
           :report "Try recompiling the shader"
           (when-let ((shader (shader-library-descriptor library)))
             (reload-shader-sources shader))
           (go start))))
    shader-id))


(defun collect-compiled-libraries (shader-name type &rest shader-opts &key &allow-other-keys)
  (when-let ((library (find-shader-library shader-name)))
    (when (clean-if-dirty library)
      (remove-from-dependencies (class-name-of (shader-library-descriptor library))
                                (shader-library-dependencies library)))
    (when (%source-of (shader-library-descriptor library))
      (let ((shader-id (if-let ((compiled-shader (apply #'find-compiled-shader
                                                        shader-name type shader-opts)))
                         compiled-shader
                         (apply #'refresh-compiled-shader library type shader-opts))))
        (nconc (list shader-id) (loop for dependency in (shader-library-dependencies library)
                                      nconc (apply #'collect-compiled-libraries
                                                   dependency type shader-opts)))))))


(defun link-shader-libraries (&rest libraries &key &allow-other-keys)
  (let ((shaders (loop for (type (shader-name . shader-opts)) on libraries by #'cddr
                       when shader-name
                         append (apply #'collect-compiled-libraries
                                       shader-name type shader-opts)))
        (program (gl:create-program)))

    (handler-bind ((serious-condition (lambda (c)
                                        (declare (ignore c))
                                        (gl:delete-program program)
                                        (setf program nil))))
      (unwind-protect
           (progn
             (loop for shader-id in shaders do
               (gl:attach-shader program shader-id))
             (gl:link-program program)
             (unless (gl:get-program program :link-status)
               (error "Program linking failed~%~A" (gl:get-program-info-log program))))
        (when program
          (loop for shader-id in shaders do
            (gl:detach-shader program shader-id)))))
    program))


(defun shader-source (shader-library shader-type)
  (if-let ((library (find-shader-library shader-library)))
    (preprocess-shader (shader-library-descriptor library) shader-type)
    (error "Library ~A not found" shader-library)))
