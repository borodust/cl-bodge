(cl:in-package :cl-bodge.resources)

;;;
;;; SCENE
;;;
(defclass scene-resource ()
  ((mesh-table :initform (make-hash-table) :reader %mesh-table-of)
   (material-table :initform (make-hash-table) :reader %material-table-of)
   (animation-table :initform (make-hash-table) :reader %animation-table-of)
   (root-node :initform nil :accessor scene-resource-root-node)))


(defun make-empty-scene-resource ()
  (make-instance 'scene-resource))


(defun scene-resource-mesh (scene id)
  (with-slots (mesh-table) scene
    (gethash id mesh-table)))


(defun (setf scene-resource-mesh) (mesh scene id)
  (with-slots (mesh-table) scene
    (setf (gethash id mesh-table) mesh)))


(defun scene-resource-material (scene id)
  (with-slots (material-table) scene
    (gethash id material-table)))


(defun (setf scene-resource-material) (material scene id)
  (with-slots (material-table) scene
    (setf (gethash id material-table) material)))


(defun scene-resource-animation (scene id)
  (with-slots (animation-table) scene
    (gethash id animation-table)))


(defun (setf scene-resource-animation) (animation scene id)
  (with-slots (animation-table) scene
    (setf (gethash id animation-table) animation)))


(defmacro do-scene-resource-meshes ((mesh id scene) &body body)
  `(maphash (lambda (,id ,mesh) (declare (ignorable ,id)) ,@body) (%mesh-table-of ,scene)))


(defmacro do-scene-resource-materials ((material id scene) &body body)
  `(maphash (lambda (,id ,material) (declare (ignorable ,id)) ,@body) (%material-table-of ,scene)))


(defmacro do-scene-resource-animations ((animation id scene) &body body)
  `(maphash (lambda (,id ,animation) (declare (ignorable ,id)) ,@body) (%animation-table-of ,scene)))


;;;
;;; MESH
;;;
(defstruct (mesh-resource
            (:constructor %make-mesh-resource))
  primitive
  position-array
  index-array
  normal-array
  tangent-array
  bone-id-array
  bone-weight-array
  (color-table (make-hash-table))
  (tex-coord-table (make-hash-table))
  (bone-array (make-array 0 :element-type '(or null mesh-resource-bone)
                            :fill-pointer t :adjustable t :initial-element nil)))


(defun add-mesh-resource-bone (mesh bone)
  (vector-push-extend bone (mesh-resource-bone-array mesh)))


(defun mesh-resource-bone (mesh bone-idx)
  (aref (mesh-resource-bone-array mesh) bone-idx))


(defun mesh-resource-bone-count (mesh)
  (fill-pointer (mesh-resource-bone-array mesh)))


(defun make-mesh-resource (primitive)
  (%make-mesh-resource :primitive primitive))


(defun mesh-resource-color-array (mesh channel-id)
  (gethash channel-id (mesh-resource-color-table mesh)))


(defun (setf mesh-resource-color-array) (color-array mesh channel-id)
  (setf (gethash channel-id (mesh-resource-color-table mesh)) color-array))


(defun mesh-resource-tex-coord-array (mesh channel-id)
  (gethash channel-id (mesh-resource-tex-coord-table mesh)))


(defun (setf mesh-resource-tex-coord-array) (tex-coord-array mesh channel-id)
  (setf (gethash channel-id (mesh-resource-tex-coord-table mesh)) tex-coord-array))


(defun for-each-mesh-resource-color-array (mesh fu)
  (maphash fu (mesh-resource-color-table mesh)))


(defun for-each-mesh-resource-tex-coord-array (mesh fu)
  (maphash fu (mesh-resource-tex-coord-table mesh)))


(defmacro do-mesh-resource-color-arrays ((array channel-id mesh) &body body)
  `(for-each-mesh-resource-color-array ,mesh (lambda (,channel-id ,array)
                                               (declare (ignorable ,channel-id))
                                               ,@body)))


(defmacro do-mesh-resource-tex-coord-arrays ((array channel-id mesh) &body body)
  `(for-each-mesh-resource-tex-coord-array ,mesh (lambda (,channel-id ,array)
                                                   (declare (ignorable ,channel-id))
                                                   ,@body)))

;;;
;;; MATERIAL
;;;
(defstruct (texture-resource
            (:constructor make-texture-resource))
  name
  channel
  coord-id
  mapping-id
  mapping-name
  mapping-mode-u
  mapping-mode-v
  mapping-filter-mag
  mapping-filter-min
  uv-transform
  scale
  strength)


(defstruct (material-resource
            (:constructor %make-material-resource))
  name
  shininess
  ambient-color
  shininess-strength
  opacity
  reflectivity
  bump-scaling
  displacement-scaling
  diffuse-color
  specular-color
  emissive-color
  transparent-color
  reflective-color
  ;; metallic-roughness
  base-color-factor
  metallic-factor
  roughness-factor
  glossiness-factor

  alpha-mode
  alpha-cutoff
  (texture-table (make-hash-table :test #'equal)))


(defun make-material-resource (&key name
                                 shininess
                                 ambient-color
                                 diffuse-color
                                 specular-color
                                 emissive-color
                                 shininess-strength
                                 transparent-color
                                 reflective-color
                                 base-color-factor
                                 metallic-factor
                                 roughness-factor
                                 glossiness-factor
                                 alpha-mode
                                 alpha-cutoff)
  (%make-material-resource :name name
                           :shininess shininess
                           :ambient-color ambient-color
                           :diffuse-color diffuse-color
                           :specular-color specular-color
                           :emissive-color emissive-color
                           :transparent-color transparent-color
                           :shininess-strength shininess-strength
                           :reflective-color reflective-color
                           :base-color-factor base-color-factor
                           :metallic-factor metallic-factor
                           :roughness-factor roughness-factor
                           :glossiness-factor glossiness-factor
                           :alpha-mode alpha-mode
                           :alpha-cutoff alpha-cutoff))


(defun material-resource-texture (material type id &optional ensure-exist)
  (let* ((key (cons type id))
         (tex-table (material-resource-texture-table material))
         (texture (gethash key tex-table)))
    (if (and (null texture) ensure-exist)
        (setf (gethash key tex-table) (make-texture-resource))
        texture)))


(defun (setf material-resource-texture) (value material type id)
  (setf (gethash (cons type id) (material-resource-texture-table material)) value))


(defmacro do-material-resource-textures ((texture type id material) &body body)
  (with-gensyms (key)
    `(maphash (lambda (,key ,texture)
                (destructuring-bind (,type . ,id) ,key
                  (declare (ignorable ,type ,id))
                  ,@body))
              (material-resource-texture-table ,material))))

;;;
;;; ANIMATION
;;;
(defstruct (animation-resource-key-sequence
            (:constructor make-animation-resource-key-sequence))
  timing-array
  value-array)


(defstruct (animation-resource-channel
            (:constructor %make-animation-resource-channel))
  node-id
  translation-sequence
  rotation-sequence
  scale-sequence
  pre-state
  post-state)


(defun make-animation-resource-channel (node-id pre-state post-state
                                        &key translation-sequence rotation-sequence scale-sequence)
  (%make-animation-resource-channel :node-id node-id
                                    :pre-state pre-state
                                    :post-state post-state
                                    :translation-sequence translation-sequence
                                    :rotation-sequence rotation-sequence
                                    :scale-sequence scale-sequence))


(defstruct (animation-resource
            (:constructor %make-animation-resource)
            (:conc-name %animation-resource-))
  duration
  channels)


(defun make-animation-resource (duration)
  (%make-animation-resource :duration duration))


(defun add-animation-resource-channel (animation channel)
  (push channel (%animation-resource-channels animation)))


(defun animation-resource-channels (animation)
  (%animation-resource-channels animation))


;;;
;;; SKELETON
;;;
(defstruct (mesh-resource-bone
            (:constructor %make-mesh-resource-bone))
  node-id
  offset)


(defun make-mesh-resource-bone (node-id offset)
  (%make-mesh-resource-bone :node-id node-id :offset offset))

;;;
;;; NODE
;;;
(defstruct (scene-resource-node
            (:constructor %make-scene-resource-node))
  id
  transform
  mesh-id-list%
  children
  (metadata-table (make-hash-table :test #'equal)))


(defun make-scene-resource-node (id transform)
  (%make-scene-resource-node :id id :transform transform))


(defun add-scene-resource-node-child (node child)
  (nconcf (scene-resource-node-children node) (list child))
  child)


(defun add-scene-resource-node-mesh-id (node mesh-id)
  (nconcf (scene-resource-node-mesh-id-list% node) (list mesh-id))
  mesh-id)


(defun scene-resource-node-mesh-id-list (node)
  (scene-resource-node-mesh-id-list% node))


(defun scene-resource-node-property (node property-name)
  (gethash property-name (scene-resource-node-metadata-table node)))


(defun (setf scene-resource-node-property) (value node property-name)
  (setf (gethash property-name (scene-resource-node-metadata-table node)) value))
