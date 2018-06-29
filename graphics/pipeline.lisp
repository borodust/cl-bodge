(cl:in-package :cl-bodge.graphics)


(defgeneric build-shading-program (pipeline))


(defclass pipeline (disposable)
  ((dirty-p :initform t)
   (vertex-array-id :initform 0)
   (last-updated :initform 0)
   (parameter-table :initform (make-hash-table))
   (shading-program :initform nil)))


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
      (log:trace "Refresing pipeline ~A" pipeline)
      (when shading-program
        (gl:delete-program shading-program))
      (refresh-parameter-table parameter-table (pipeline-shaders pipeline))
      (setf shading-program (build-shading-program pipeline)
            last-updated (real-time-seconds)
            dirty-p nil))))


(defmethod initialize-instance :after ((this pipeline) &key)
  (with-slots (vertex-array-id) this
    (setf vertex-array-id (gl:gen-vertex-array)))
  (ensure-clean-pipeline this))


(defun make-pipeline (pipeline-name)
  (make-instance pipeline-name))


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


(defun %build-shading-program (&key vertex fragment geometry)
  (let (program)
    (tagbody start
       (restart-case
           (setf program (link-shader-libraries :vertex-shader vertex
                                                :fragment-shader fragment
                                                :geometry-shader geometry))
         (relink ()
           :report "Try relinking the pipeline"
           (go start))))
    program))


(defmacro defpipeline (name-and-opts &body shaders)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (destructuring-bind (&key (primitive '(:triangles))) (alist-plist opts)
      (with-gensyms (this)
        `(progn
           (defclass ,name (pipeline) ())
           (defmethod pipeline-primitive ((,this ,name))
             (declare (ignore ,this))
             ,@primitive)
           (defmethod pipeline-shaders ((,this ,name))
             (declare (ignore ,this))
             ',(loop for (nil shader-name) on shaders by #'cddr
                     collect shader-name))
           (defmethod build-shading-program ((,this ,name))
             (declare (ignore ,this))
             (%build-shading-program ,@(loop for (type shader-name) on shaders by #'cddr
                                             append `(,type ',shader-name))))
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
              when value
              do (multiple-value-bind (parameters exist-p)
                     (program-input-parameters pipeline name)
                   (when exist-p
                     (apply #'inject-shader-input value parameters))))))))
