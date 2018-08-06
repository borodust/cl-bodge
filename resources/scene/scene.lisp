(cl:in-package :cl-bodge.resources)

;;;
;;; SCENE
;;;
(defclass scene-resource ()
  ((mesh-table :initform (make-hash-table) :reader %mesh-table-of)
   (material-table :initform (make-hash-table) :reader %material-table-of)))


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


(defmacro do-scene-resource-meshes ((mesh id scene) &body body)
  `(maphash (lambda (,id ,mesh) (declare (ignorable ,id)) ,@body) (%mesh-table-of ,scene)))


(defmacro do-scene-resource-materials ((material id scene) &body body)
  `(maphash (lambda (,id ,material) (declare (ignorable ,id)) ,@body) (%material-table-of ,scene)))

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
  (color-table (make-hash-table))
  (tex-coord-table (make-hash-table)))


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

;;;p
;;; MATERIAL
;;;
(defstruct texture-resource
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
  strength)


(defstruct material-resource
  name
  shininess
  diffuse-color
  emissive-color
  ;; metallic-roughness
  base-color-factor
  metallic-factor
  roughness-factor
  glossiness-factor

  alpha-mode
  alpha-cutoff
  (texture-table (make-hash-table :test #'equal)))


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
