(cl:in-package :cl-bodge.resources)

(declaim (special *scene-read-buffer*
                  *scene*))

(defparameter *scene-read-buffer-size* (* 64 1024))


(defmacro with-character-stream ((stream &key (external-format :utf-8)) &body body)
  `(let ((,stream (flexi-streams:make-flexi-stream ,stream :external-format ,external-format)))
     ,@body))


(defun read-descriptor (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (block nil
        (handler-bind ((end-of-file (lambda (c) (declare (ignore c)) (return))))
          (read-preserving-whitespace stream nil nil nil))))))


(defun for-each-descriptor (stream action)
  (with-character-stream (stream)
    (loop for descriptor = (read-descriptor stream)
          while descriptor
          do (destructuring-bind (descriptor-type &rest args &key &allow-other-keys)
                 descriptor
               (apply action descriptor-type args)))))


(defmacro do-descriptors ((descriptor-lambda-list stream) &body body)
  (with-gensyms (args)
    `(for-each-descriptor ,stream (lambda (&rest ,args)
                                    (destructuring-bind (,@descriptor-lambda-list) ,args
                                      ,@body)))))


(defun read-array (stream &key (length (error ":length missing"))
                            ((:type element-type) (error ":element-type missing"))
                            &allow-other-keys)
  (let* ((type (ecase element-type
                 (:float 'single-float)
                 (:unsigned-int '(unsigned-byte 32))
                 (:int '(signed-byte 32))))
         (byte-length (* length (cffi:foreign-type-size element-type)))
         (array (make-array length :element-type type)))
    (with-simple-array-pointer (dst-ptr array)
      (loop with byte-offset = 0
            with dst-addr = (cffi:pointer-address dst-ptr)
            with src-ptr = (static-vectors:static-vector-pointer *scene-read-buffer*)
            for bytes-read = (read-sequence *scene-read-buffer* stream
                                            :end (min (- byte-length byte-offset)
                                                      *scene-read-buffer-size*))
            while (> bytes-read 0)
            do (%libc.es:memcpy (cffi:make-pointer (+ dst-addr byte-offset))
                                src-ptr
                                bytes-read)
               (incf byte-offset bytes-read)
            finally (unless (= byte-offset byte-length)
                      (error "Premature end of stream: expected ~A, got ~A"
                             byte-length byte-offset))))
    array))


(defun read-bone (&key index node-id offset)
  (declare (ignore index))
  (make-mesh-resource-bone node-id (sequence->mat4 offset)))


(defun read-mesh (stream &key index primitive size)
  (let ((stream (make-bounded-input-stream stream size))
        (mesh (make-mesh-resource primitive))
        (bones (make-array 0 :fill-pointer t :adjustable t)))
    (do-descriptors ((attribute-type &rest args &key channel-id &allow-other-keys) stream)
      (ecase attribute-type
        (:position-array (setf (mesh-resource-position-array mesh) (apply #'read-array stream args)))
        (:index-array (setf (mesh-resource-index-array mesh) (apply #'read-array stream args)))
        (:normal-array (setf (mesh-resource-normal-array mesh) (apply #'read-array stream args)))
        (:tangent-array (setf (mesh-resource-tangent-array mesh) (apply #'read-array stream args)))
        (:bone-id-array (setf (mesh-resource-bone-id-array mesh) (apply #'read-array stream args)))
        (:bone-weight-array (setf (mesh-resource-bone-weight-array mesh) (apply #'read-array stream args)))
        (:color-array (setf (mesh-resource-color-array mesh channel-id) (apply #'read-array stream args)))
        (:tex-coord-array
         (setf (mesh-resource-tex-coord-array mesh channel-id) (apply #'read-array stream args)))
        (:bone (vector-push-extend args bones))))
    (loop for bone-args across (sort bones #'< :key (lambda (bone-args)
                                                      (getf bone-args :index)))
          for expected-index from 0
          if (= expected-index (getf bone-args :index))
            do (add-mesh-resource-bone mesh (apply #'read-bone bone-args))
          else
            do (error "Bone with index ~A missing" expected-index))
    (values mesh index)))


(defun read-material (stream &key index
                               name
                               shininess
                               diffuse-color
                               emissive-color
                               base-color-factor
                               metallic-factor
                               roughness-factor
                               glossiness-factor
                               alpha-mode
                               alpha-cutoff
                               textures &allow-other-keys)
  (declare (ignore stream))
  (let ((material (make-material-resource :name name
                                          :shininess shininess
                                          :diffuse-color diffuse-color
                                          :emissive-color emissive-color
                                          :base-color-factor base-color-factor
                                          :metallic-factor metallic-factor
                                          :roughness-factor roughness-factor
                                          :glossiness-factor glossiness-factor
                                          :alpha-mode alpha-mode
                                          :alpha-cutoff alpha-cutoff)))
    (loop for texture in textures
          do (destructuring-bind (&key type
                                    id
                                    name
                                    channel
                                    coord-id
                                    mapping-id
                                    mapping-name
                                    mapping-mode-u
                                    mapping-mode-v
                                    mapping-filter-mag
                                    mapping-filter-min
                                    scale
                                    strength
                                  &allow-other-keys)
                 texture
               (setf (material-resource-texture material type id)
                     (make-texture-resource
                      :name               name
                      :channel            channel
                      :coord-id           coord-id
                      :mapping-id         mapping-id
                      :mapping-name       mapping-name
                      :mapping-mode-u     mapping-mode-u
                      :mapping-mode-v     mapping-mode-v
                      :mapping-filter-mag mapping-filter-mag
                      :mapping-filter-min mapping-filter-min
                      :scale              scale
                      :strength           strength))))
    (values material index)))


(defun parse-metadata-value (value-descriptor)
  (let ((value (second value-descriptor)))
    (ecase (first value-descriptor)
      (:vec3 (sequence->vec3 value))
      (:t value))))


(defun assign-children (node-table)
  (loop for (node . children) being the hash-value of node-table
        do (loop for child-id in children
                 for child = (first (gethash child-id node-table))
                 if child
                   do (add-scene-resource-node-child node child)
                 else
                   do (error "Child node with id ~A not found" child-id))))


(defun read-node-tree (serialized-node-list root-id)
  (let ((node-table (make-hash-table))
        root)
    (loop for node-arg in serialized-node-list
          do (destructuring-bind (&key id transform mesh-id-list children metadata) node-arg
               (let ((node (make-scene-resource-node id (sequence->mat4 transform))))
                 (loop for mesh-id in mesh-id-list
                       do (add-scene-resource-node-mesh-id node mesh-id))
                 (loop for (name . value) in metadata
                       do (setf (scene-resource-node-property node name) (parse-metadata-value value)))
                 (setf (gethash id node-table) (cons node children))
                 (when (= root-id id)
                   (setf root node)))))
    (assign-children node-table)
    root))

;;;
;;;
;;;
(defgeneric read-next (reader stream type &key &allow-other-keys))
(defgeneric flush-reader (reader)
  (:method (reader) (declare (ignore reader))))



;;;
;;; ANIMATION READER
;;;
(defclass animation-reader ()
  (animation
   (current-channel :initform nil)))


(defmethod initialize-instance :after ((this animation-reader) &key id duration)
  (with-slots (animation) this
    (setf animation (make-animation-resource duration)
          (scene-resource-animation *scene* id) animation)))


(defun flush-animation-channel (animation channel)
  (when channel
    (add-animation-resource-channel animation channel)))


(defmethod read-next ((this animation-reader) stream (type (eql :animation-channel))
                      &key node-id pre-state post-state)
  (with-slots (animation current-channel) this
    (flush-animation-channel animation current-channel)
    (setf current-channel (make-animation-resource-channel node-id pre-state post-state))))


(defun ensure-key-sequence (current-channel transform-type)
  (flet ((%ensure-key-sequence (channel seq-getter seq-setter)
           (if-let ((sequence (funcall seq-getter channel)))
             sequence
             (funcall seq-setter (make-animation-resource-key-sequence) channel))))
    (ecase transform-type
      (:rotation
       (%ensure-key-sequence current-channel
                             #'animation-resource-channel-rotation-sequence
                             #'(setf animation-resource-channel-rotation-sequence)))
      (:translation
       (%ensure-key-sequence current-channel
                             #'animation-resource-channel-translation-sequence
                             #'(setf animation-resource-channel-translation-sequence)))
      (:scale
       (%ensure-key-sequence current-channel
                             #'animation-resource-channel-scale-sequence
                             #'(setf animation-resource-channel-scale-sequence))))))


(defun read-and-set-keyframe-array (stream channel transform-type array-args setter)
  (let ((array (apply #'read-array stream array-args))
        (sequence (ensure-key-sequence channel transform-type)))
    (funcall setter array sequence)))


(defmethod read-next ((this animation-reader) stream (type (eql :keyframe-timing-array))
                      &rest args &key transform-type &allow-other-keys)
  (with-slots (current-channel) this
    (read-and-set-keyframe-array stream current-channel transform-type args
                                 #'(setf animation-resource-key-sequence-timing-array))))


(defmethod read-next ((this animation-reader) stream (type (eql :keyframe-value-array))
                      &rest args &key transform-type &allow-other-keys)
  (with-slots (current-channel) this
    (read-and-set-keyframe-array stream current-channel transform-type args
                                 #'(setf animation-resource-key-sequence-value-array))))


(defmethod flush-reader ((this animation-reader))
  (with-slots (animation current-channel) this
    (flush-animation-channel animation current-channel)))


;;;
;;; NODE GRAPH READER
;;;
(defclass node-graph-reader ()
  ((node-list :initform nil)
   (root-id :initarg :root-node-id :initform (error ":root-node-id missing"))))


(defmethod read-next ((this node-graph-reader) stream (type (eql :node))
                      &rest params &key &allow-other-keys)
  (with-slots (node-list) this
    (push params node-list)))


(defmethod flush-reader ((this node-graph-reader))
  (with-slots (node-list root-id) this
    (let ((root-node (read-node-tree node-list root-id)))
      (unless root-node
        (error "Root node with id ~A not found" root-id))
      (setf (scene-resource-root-node *scene*) root-node))))

;;;
;;; MATERIAL READER
;;;
(defclass material-reader () ())


(defmethod initialize-instance :after ((this material-reader)
                                       &key index
                                         name
                                         shininess
                                         diffuse-color
                                         emissive-color
                                         base-color-factor
                                         metallic-factor
                                         roughness-factor
                                         glossiness-factor
                                         alpha-mode
                                         alpha-cutoff
                                         textures
                                       &allow-other-keys)
  (let ((material (make-material-resource :name name
                                          :shininess shininess
                                          :diffuse-color diffuse-color
                                          :emissive-color emissive-color
                                          :base-color-factor base-color-factor
                                          :metallic-factor metallic-factor
                                          :roughness-factor roughness-factor
                                          :glossiness-factor glossiness-factor
                                          :alpha-mode alpha-mode
                                          :alpha-cutoff alpha-cutoff)))
    (loop for texture in textures
          do (destructuring-bind (&key type
                                    id
                                    name
                                    channel
                                    coord-id
                                    mapping-id
                                    mapping-name
                                    mapping-mode-u
                                    mapping-mode-v
                                    mapping-filter-mag
                                    mapping-filter-min
                                    scale
                                    strength
                                  &allow-other-keys)
                 texture
               (setf (material-resource-texture material type id)
                     (make-texture-resource
                      :name               name
                      :channel            channel
                      :coord-id           coord-id
                      :mapping-id         mapping-id
                      :mapping-name       mapping-name
                      :mapping-mode-u     mapping-mode-u
                      :mapping-mode-v     mapping-mode-v
                      :mapping-filter-mag mapping-filter-mag
                      :mapping-filter-min mapping-filter-min
                      :scale              scale
                      :strength           strength))))
    (setf (scene-resource-material *scene* index) material)))


;;;
;;; MESH READER
;;;
(defclass mesh-reader ()
  (mesh
   (bones :initform (make-array 0 :fill-pointer t :adjustable t))))


(defmethod initialize-instance :after ((this mesh-reader) &key index primitive &allow-other-keys)
  (with-slots (mesh) this
    (setf mesh (make-mesh-resource primitive)
          (scene-resource-mesh *scene* index) mesh)))


(defmethod flush-reader ((this mesh-reader))
  (with-slots (mesh bones) this
    (loop for bone-args across (sort bones #'< :key (lambda (bone-args)
                                                      (getf bone-args :index)))
          for expected-index from 0
          if (= expected-index (getf bone-args :index))
            do (add-mesh-resource-bone mesh (apply #'read-bone bone-args))
          else
            do (error "Bone with index ~A missing" expected-index))))


(defmethod read-next ((this mesh-reader) stream (type (eql :position-array)) &rest args &key)
  (with-slots (mesh) this
    (setf (mesh-resource-position-array mesh) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :index-array)) &rest args &key)
  (with-slots (mesh) this
    (setf (mesh-resource-index-array mesh) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :normal-array)) &rest args &key)
  (with-slots (mesh) this
    (setf (mesh-resource-normal-array mesh) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :tangent-array)) &rest args &key)
  (with-slots (mesh) this
    (setf (mesh-resource-tangent-array mesh) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :bone-id-array)) &rest args &key)
  (with-slots (mesh) this
    (setf (mesh-resource-bone-id-array mesh) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :bone-weight-array)) &rest args &key)
  (with-slots (mesh) this
    (setf (mesh-resource-bone-weight-array mesh) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :color-array)) &rest args &key channel-id)
  (with-slots (mesh) this
    (setf (mesh-resource-color-array mesh channel-id) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :tex-coord-array)) &rest args &key channel-id)
  (with-slots (mesh) this
    (setf (mesh-resource-tex-coord-array mesh channel-id) (apply #'read-array stream args))))


(defmethod read-next ((this mesh-reader) stream (type (eql :bone)) &rest args &key)
  (with-slots (bones) this
    (vector-push-extend args bones)))


;;;
;;; SCENE READER
;;;

(defclass scene-reader ()
  ((scene :initform (make-empty-scene-resource))
   (property-reader :initform nil)))


(defun build-scene (scene-reader)
  (with-slots (scene) scene-reader
    scene))


(defmethod read-next :around ((this scene-reader) stream type &key)
  (with-slots (scene) this
    (let ((*scene* scene))
      (call-next-method))))


(defun switch-scene-property-reader (scene-reader reader-class params)
  (with-slots ((this-property-reader property-reader)) scene-reader
    (when this-property-reader
      (flush-reader this-property-reader))
    (setf this-property-reader (apply #'make-instance reader-class params))))


(defmethod read-next ((this scene-reader) stream (type (eql :mesh)) &rest args &key)
  (switch-scene-property-reader this 'mesh-reader args))


(defmethod read-next ((this scene-reader) stream (type (eql :material)) &rest args &key)
  (switch-scene-property-reader this 'material-reader args))


(defmethod read-next ((this scene-reader) stream (type (eql :node-graph)) &rest args &key)
  (switch-scene-property-reader this 'node-graph-reader args))


(defmethod read-next ((this scene-reader) stream (type (eql :animation)) &rest args &key)
  (switch-scene-property-reader this 'animation-reader args))


(defmethod read-next ((this scene-reader) stream type &rest args &key)
  (with-slots (property-reader) this
    (apply #'read-next property-reader stream type args)))


(defun read-scene (stream)
  (static-vectors:with-static-vector (*scene-read-buffer* *scene-read-buffer-size*)
    (let ((reader (make-instance 'scene-reader)))
      (do-descriptors ((type &rest params) stream)
        (apply #'read-next reader stream type params))
      (flush-reader reader)
      (build-scene reader))))
