(cl:in-package :cl-bodge.graphics)


(defgeneric build-shading-program (pipeline))


(defclass pipeline (disposable)
  ((dirty-p :initform t)
   (vertex-array-id :initform 0)
   (last-updated :initform 0)
   (parameter-table :initform (make-hash-table))
   (shading-program :initform nil)
   (defines :initarg :options :initform nil)))


(defgeneric pipeline-primitive (pipeline))
(defgeneric pipeline-shaders (pipeline))


(define-destructor pipeline (shading-program)
  (dispose-gl-object shading-program #'gl:delete-program))


(defun pipeline-shaders-outdated-p (pipeline)
  (with-slots (last-updated) pipeline
    (loop for shader-class in (pipeline-shaders pipeline)
            thereis (shader-library-dirty-p (find-shader-library shader-class)))))


(defun refresh-parameter-table (table shaders)
  (clrhash table)
  (loop for shader-class in shaders
        do (loop for parameters in (shader-library-parameters shader-class)
                 do (destructuring-bind (name &rest opts) parameters
                      (setf (gethash name table) opts)))))


(defun ensure-clean-pipeline (pipeline)
  (with-slots (dirty-p last-updated shading-program parameter-table) pipeline
    (when (or dirty-p (pipeline-shaders-outdated-p pipeline))
      (log/debug "Refresing pipeline ~A" pipeline)
      (when shading-program
        (gl:delete-program shading-program))
      (setf shading-program (build-shading-program pipeline)
            last-updated (real-time-seconds)
            dirty-p nil)
      (refresh-parameter-table parameter-table (pipeline-shaders pipeline)))))


(defmethod initialize-instance :after ((this pipeline) &key)
  (with-slots (vertex-array-id) this
    (setf vertex-array-id (gl:gen-vertex-array)))
  (ensure-clean-pipeline this))


(defgeneric render-pipeline (pipeline &key &allow-other-keys))


(defun %render-pipeline (pipeline index-buffer instance-count
                        vertex-count vertex-offset primitive
                        &rest input)
  (enable-pipeline pipeline input)
  (let ((mode (or primitive (pipeline-primitive pipeline)))
        (vertex-count (or vertex-count (index-buffer-length index-buffer) 0)))
    (if index-buffer
        (progn
          (inject-shader-input index-buffer)
          (%gl:draw-elements-instanced mode
                                       vertex-count
                                       :unsigned-int
                                       (cffi:make-pointer vertex-offset)
                                       instance-count))
        (%gl:draw-arrays-instanced mode vertex-offset vertex-count instance-count))))


(defmethod render-pipeline ((this pipeline) &rest input
                            &key index-buffer instance-count
                              vertex-count vertex-offset primitive)
  (apply #'%render-pipeline this index-buffer instance-count
         vertex-count vertex-offset primitive input))


(defun make-shader-pipeline (pipeline-name &rest options &key &allow-other-keys)
  (make-instance pipeline-name :options options))


(defun program-input-parameters (pipeline variable-name)
  (with-slots (parameter-table) pipeline
    (gethash variable-name parameter-table)))


(defmethod update-instance-for-redefined-class :after ((this pipeline)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (dirty-p last-updated) this
    (setf dirty-p t
          last-updated (real-time-seconds))))


(defun bake-pipeline (pipeline &rest partial-input)
  (declare (ignore pipeline partial-input))
  (error "Unimplemented yet"))


(defun %%build-shading-program (this vertex fragment geometry)
  (with-slots (defines) this
    (let ((combined-defines (append defines (%defines-of this))))
      (link-shader-libraries :vertex-shader (append vertex combined-defines)
                             :fragment-shader (append fragment combined-defines)
                             :geometry-shader (when geometry
                                                (append geometry combined-defines))))))


(defun %build-shading-program (this &key vertex fragment geometry)
  (restart-case
      (%%build-shading-program this vertex fragment geometry)
    (rebuild ()
      :report "Try rebuilding the pipeline"
      (build-shading-program this))
    (clear-cache-and-rebuild ()
      :report "Reset whole shader cache and try rebuilding the pipeline"
      (clear-registry-cache)
      (reload-all-shader-sources)
      (build-shading-program this))))


(defmacro defpipeline (name-and-opts &body shaders)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (destructuring-bind (&key options (primitive '(:triangles))) (alist-plist opts)
      (with-gensyms (this)
        `(progn
           (defclass ,name (pipeline) ())
           (defmethod pipeline-primitive ((,this ,name))
             (declare (ignore ,this))
             ,@primitive)
           (defmethod %defines-of ((,this ,name))
             (list ,@options))
           (defmethod pipeline-shaders ((,this ,name))
             (declare (ignore ,this))
             ',(loop for (nil shader-opts) on shaders by #'cddr
                     collect (first (ensure-list shader-opts))))
           (defmethod build-shading-program ((,this ,name))
             (%build-shading-program ,this
                                     ,@(loop for (type shader-args) on shaders by #'cddr
                                             for (shader-name . shader-opts) = (ensure-list shader-args)
                                             append `(,type (list ',shader-name ,@shader-opts)))))
           (make-instances-obsolete ',name))))))


(defun enable-pipeline (pipeline input)
  (with-slots (shading-program vertex-array-id) pipeline
    (ensure-clean-pipeline pipeline)
    (gl:use-program shading-program)
    (let ((*active-shading-program* shading-program))
      (with-texture-units ()
        (gl:bind-vertex-array vertex-array-id)
        (gl:bind-buffer :element-array-buffer 0)
        (loop for (name value) on input by #'cddr
              do (multiple-value-bind (parameters exist-p)
                     (program-input-parameters pipeline name)
                   (when exist-p
                     (apply #'inject-shader-input value parameters))))))))
